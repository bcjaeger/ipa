
#' Nearest neighbor imputation
#'
#' This function conducts nearest neighbor imputation with the added option
#'   of using a sequence of neighbor values instead of picking one. One
#'   imputed dataset is created for each value of nearest neighbors
#'   (`k`).
#'
#' @param data_ref a data frame.
#'
#' @param data_new an optional data frame. If supplied, then `data_ref`
#'   will be used as a reference dataset for `data_new` and the output
#'   will contain imputed values for `data_new`. If not supplied,
#'   the output will contain imputed values for `data_ref`.
#'
#' @param cols columns that should be imputed and/or used to impute other
#'   columns. Supports tidy select functions (see examples).
#'
#' @param k_neighbors a numeric vector indicating how many neighbors should
#'   be used to impute missing values.
#'
#' @param aggregate a logical value. If `TRUE`, then neighbors
#'   will be aggregated to generate imputations. If `FALSE`, then one
#'   neighbor will be sampled at random to generate a missing value. Using
#'   `aggregate = FALSE` can be helpful if you are conducting
#'   multiple imputation.
#'
#' @param fun_aggr_ctns a function used to aggregate neighbors for continuous
#'   variables. If unspecified, the `mean()` function is used.
#'
#' @param fun_aggr_intg a function used to aggregate neighbors for integer
#'   values variables. If unspecified, the `medn_est()` function is used.
#'   This function returns the median of neighbor values, rounded to the
#'   nearest integer. `medn_est_conserve()` goes one step further and
#'   identifies which neighbor value is closest to the median, and returns
#'   that value. Both of these options can be helpful for integer valued
#'   columns if you want to make sure the imputed values do not contain
#'   impossible quantities, e.g. no. of children = 3/4.
#'
#' @param fun_aggr_catg a function used to aggregate neighbors for categorical
#'   variables. If unspecified, the `mode_est()` function is used.
#'
#' @param epsilon Computed numbers (variable ranges) smaller than eps are
#'   treated as zero
#'
#' @param nthread Number of threads to use for parallelization. By default,
#'   for a dual-core machine, 2 threads are used. For any other machine n-1
#'   cores are used so your machine doesn't freeze during a big computation.
#'   The maximum nr of threads are determined using omp_get_max_threads
#'   at C level.
#'
#' @param verbose logical value. If `TRUE`, output is printed on the
#'   console. If `FALSE`, nothing is printed.
#'
#' @return a list of imputed datasets the same length as `k_neighbors`.
#'
#' @export
#'
#' @examples
#'
#' data(diabetes, package = 'ipa')
#'
#' trn <- diabetes$missing[1:25, ]
#' tst <- diabetes$missing[26:50, ]
#'
#' trn_imputes <- impute_nbrs(data_ref = trn, k = 1:5)
#' tst_imputes <- impute_nbrs(data_ref = trn, data_new = tst, k = 1:5)
#'
impute_nbrs <- function(
  data_ref,
  data_new = NULL,
  cols = dplyr::everything(),
  k_neighbors = 10,
  aggregate = TRUE,
  fun_aggr_ctns = NULL,
  fun_aggr_intg = NULL,
  fun_aggr_catg = NULL,
  nthread = getOption("gd_num_thread"),
  epsilon = 1e-08,
  verbose = FALSE
) {

  # Safety checks start:
  check_min_lax(k_neighbors, value = 0,
    label = 'neighbor count (k_neighbors)')

  check_bool(aggregate, label = 'neighbor aggregation (aggregate)')
  check_l1_stop(aggregate, label = 'neighbor aggregation (aggregate)')

  check_min_lax(epsilon, value = 0, label = 'epsilon')
  check_min_strict(epsilon, value = 0, label = 'epsilon')

  check_bool(verbose, label = 'verbosity')
  check_l1_stop(verbose, label = 'verbosity')

  # keep_cols = columns to be imputed
  keep_cols <- names(data_ref) %>%
    tidyselect::vars_select(!!rlang::enquo(cols))

  if(length(keep_cols) == 1)
    stop("1 column was selected (", keep_cols,
      ") but 2+ are needed", call. = FALSE)


  # convert data frames into data.table objects if needed
  # subset to the columns we are imputing

  if(!is.data.table(data_ref))
    DT_ref <- as.data.table(data_ref)[, ..keep_cols]
  else
    DT_ref <- data_ref[, ..keep_cols]

  # the row numbers that contain missing data for each keep_col
  miss_indx <- mindx(DT_ref, drop_empty = FALSE)
  # the number of missing values in each row
  check_missingness(miss_indx, N = nrow(DT_ref),
    P = ncol(DT_ref), label = 'reference data')
  # variable types should be...
  vt <- c('numeric', 'integer', 'logical', 'character', 'factor')
  # check that the columns in DT_ref fit this constraint
  check_var_types(DT_ref, valid_types = vt)

  # initialize a null DT_new object in case there isn't any new data
  DT_new <- NULL


  # If new data are not supplied, impute data_ref using miss_indx.
  # If new data are supplied, impute data_new and re-create miss_indx
  # --- Also, run the same tests on data_new
  if(!is.null(data_new)){

    # convert data frames into data.table objects if needed
    # subset to the columns we are imputing
    if(!is.data.table(data_new)){
      DT_new <- as.data.table(data_new)[, ..keep_cols]
    } else {
      DT_new <- data_new[, ..keep_cols]
    }

    # protect against errors due to factors with different levels
    # in DT_new versus data_ref. Since data_ref is used to impute,
    # any levels of factors not in data_ref should be considered
    # missing (and then imputed).

    for(col in keep_cols){
      if(is.factor(DT_new[[col]])){

        set(DT_new, j = col,
          value = factor(DT_new[[col]],
            levels = levels(DT_ref[[col]]))
        )

      }
    }

    # should have exactly the Same names and types as reference data
    check_data_new_names(DT_ref, DT_new)
    check_data_new_types(DT_ref, DT_new)

    # the row numbers that contain missing data for each keep_col
    miss_indx <- mindx(DT_new, drop_empty = FALSE)

  }

  # Safety checks done

  # identify which columns (if any) don't need to be imputed
  complete_cols <- which(sapply(miss_indx, is_empty))
  # if there are complete columns, remove them from missing index
  if(!is_empty(complete_cols)) miss_indx <- miss_indx[-complete_cols]
  # initialize the list of imputed values (the function's output)
  imputed_values <- vector(mode = 'list', length = length(k_neighbors))

  for(i in seq_along(imputed_values)) {
    # each item in imputed values is a list
    imputed_values[[i]] <- vector(mode = 'list', length = length(miss_indx))
    names(imputed_values[[i]]) <- names(miss_indx)
  }

  for(impute_col in names(miss_indx)){

    var_type <- class(DT_ref[[impute_col]])[1]

    aggregate_function <- if(aggregate){

      switch(
        var_type,
        'numeric'   = fun_aggr_ctns %||% mean,
        'integer'   = fun_aggr_intg %||% medn_est,
        'logical'   = fun_aggr_catg %||% mode_est,
        'character' = fun_aggr_catg %||% mode_est,
        'factor'    = fun_aggr_catg %||% mode_est,
        stop(impute_col, " has unsupported variable type ", var_type,
          call. = FALSE)
      )

    } else {

      # if user has indicated that they do not want to aggregate
      # neighbor values, then sample one value at random from the
      # set of nearest neighbors
      function(x) sample(x, size = 1)

    }

    # initial set of predictors:
    # everything except the column we are imputing
    impute_prds <- base::setdiff(keep_cols, impute_col)

    # rows for imputation - reference data
    # keep rows that have data for the column we want to impute.
    irows_ref <- which(!is.na(DT_ref[[impute_col]]))
    # rows for imputation - new data
    # keep rows that are missing (i.e., the ones we need to impute)
    irows_new <- miss_indx[[impute_col]]

    gwr_n <- max(k_neighbors)

    if(length(irows_ref) < gwr_n){

      ### rationale for the error below:
      # sometimes you can't get enough neighbors because the data
      # have too few observations. If this is the case, we'd rather
      # not throw a hard error and stop the entire imputation procedure,
      # but we do need to say that the number of neighbors requested
      # exceeded the number of observed cases. HOWEVER, if we do not throw
      # the error, then the output will be very hard to manage and possibly
      # incorrect (i.e., it may say we used 20 neighbors when we only use 8).
      stop("some values of k_neighbors (",
        list_things(k_neighbors[k_neighbors>=length(irows_ref)]),
        ") exceed or match the number", " of observed values (",
        length(irows_ref), ") for ", impute_col, ".", call. = FALSE)

    }

    if(verbose) message(
      glue::glue("Imputing {impute_col}, N observed = {length(irows_ref)}")
    )


    # identify nearest neighbors via gower's distance
    if(!is.null(DT_new)){
      gwr_topn <- gower::gower_topn(
          x = DT_new[irows_new, ..impute_prds],
          y = DT_ref[irows_ref, ..impute_prds],
          n = gwr_n,
          eps = epsilon,
          nthread = nthread
        )$index
    } else {
      gwr_topn <- gower::gower_topn(
        x = DT_ref[irows_new, ..impute_prds],
        y = DT_ref[irows_ref, ..impute_prds],
        n = gwr_n,
        eps = epsilon,
        nthread = nthread
      )$index
    }


    # above, we filtered DT_ref instead of creating a copy and subsetting.
    # This saves memory, but causes the index from gower_topn to correspond
    # with the indices of the filtered DT_ref instead of DT_ref. The code
    # here back-transforms those indices and overwrites gower_topn with a
    # version that corresponds to DT_ref.
    gwr_topn <- matrix(irows_ref[gwr_topn], ncol = ncol(gwr_topn))

    # modify gower_topn in place for efficiency.
    # loop through the columns, replacing each set of indices with
    # the corresponding imputed values from DT_ref
    for(i in seq(nrow(gwr_topn))){
      gwr_topn[i, ] <- DT_ref[[impute_col]][ as.numeric(gwr_topn[i, ]) ]
    }


    impute_vals <- lapply(k_neighbors, function(x){
      apply(gwr_topn[seq(x), , drop = FALSE], 2, aggregate_function)
    })

    # factor values are converted to integers when they get passed to
    # gwr_vals - convert them back to the given levels of impute_vals.
    if(var_type == 'factor'){

      labels <- levels(DT_ref[[impute_col]])
      names(labels) <- seq(length(labels))

      impute_vals <- purrr::map(
        .x = impute_vals,
        .f = ~ factor(dplyr::recode(.x, !!!labels), levels = labels)
      )

    }

    for(i in seq_along(imputed_values)){

      imputed_values[[i]][[impute_col]] <- impute_vals[[i]]

    }

  }

  data.table(
    impute = seq_along(k_neighbors),
    k_neighbors = k_neighbors,
    imputed_values = imputed_values
  )

}

#' Neighbor aggregates
#'
#' @param x a vector of character or integer values for `mode_est`. For
#'   `medn_est`, only integer values are allowed.
#' @param random_ties a logical value indicating whether ties should be
#'   broken at random when two values occur at the same frequency in `x`.
#'
#' @return an aggregate scalar with the same type as `x`.
#'
#' @export
#'
#' @examples
#'
#' x <- c('a', 'a', 'b')
#' mode_est(x)
#'
#' x <- c(1L, 1L, 2L)
#' medn_est(x)
#'
#'
#'
mode_est <- function(x, random_ties = FALSE){

  stopifnot(is.character(x) | is.integer(x))

  if(any(is.na(x)))
    stop("missing values should not be passed to mode_est",
      call. = FALSE)

  tab <- table(x)
  modes <- names(tab)[tab == max(tab)]

  if(random_ties){
    sample(modes, size = 1)
  } else {
    modes[1L]
  }

}

#' @rdname mode_est
#' @export
medn_est <- function(x){

  stopifnot(is.integer(x))

  if(any(is.na(x)))
    stop("missing values should not be passed to medn_est",
      call. = FALSE)

  as.integer(round(median(x), digits = 0))

}

#' @rdname mode_est
#' @export
medn_est_conserve <- function(x){

  output <- medn_est(x)

  if(output %in% x){
    return(output)
  }

  x[which.min(abs(x-output))]

}


