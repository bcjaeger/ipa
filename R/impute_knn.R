

# Internal functions ------------------------------------------------------

impute_knn_col <- function(data_ref, impute_col,
  data_new = NULL, k_neighbors = 10, aggregate_neighbors = TRUE,
  fun_aggr_ctns = NULL, fun_aggr_intg = NULL, fun_aggr_catg = NULL,
  nthread = getOption("gd_num_thread"), epsilon = 1e-8, verbose = 0
){

  var_type <- class(data_ref[[impute_col]])[1]

  aggregate_function <- switch(
    var_type,
    'numeric'   = fun_aggr_ctns %||% mean,
    'integer'   = fun_aggr_intg %||% medn_est,
    'logical'   = fun_aggr_catg %||% mode_est,
    'character' = fun_aggr_catg %||% mode_est,
    'factor'    = fun_aggr_catg %||% mode_est,
    stop(impute_col, " has unsupported variable type ", var_type,
      call. = FALSE)
  )

  # if user has indicated that they do not want to aggregate
  # neighbor values, then overwrite the aggregate_function so
  # that it will sample one value at random from the set of
  # nearest neighbors
  if(!aggregate_neighbors)
    aggregate_function <- function(x) sample(x, size = 1)

  new_data_supplied <- !is.null(data_new)
  # data_out is not modified until imputation is complete
  # data_new is modified by selection of columns for imputing
  # data_ref is modified by selection of columns and rows for imputing
  # if user supplies new_data, then neighbors are found only in data_ref
  data_out <- data_new <- data_new %||% data_ref

  # if there aren't any missing values, just return a list of
  # the observed data, replicated a number of times equal to the
  # length of k.
  if(!any(is.na(data_new[[impute_col]]))){
    return(purrr::map(k_neighbors, ~ data_new[[impute_col]]))
  }

  # protect against errors due to the imputed variable having
  # different levels in data_new versus data_ref. Since data_ref
  # is solely used to impute values in data_new, any levels of
  # impute_col that occur only in data_new should be considered
  # missing (and then imputed). This can be prevented by applying
  # step_other or lumping uncommon categories into an 'other' level.
  if(var_type == 'factor'){
    levels(data_new[[impute_col]]) <- levels(data_ref[[impute_col]])
  }

  stopifnot(all(names(data_ref) == names(data_new)))

  # initial set of predictors:
  # everything except the column we are imputing
  impute_prds <- base::setdiff(names(data_ref), impute_col)

  # initial rows for imputation - reference data
  # keep rows that have data for the column we want to impute.
  data_ref %<>% tidyr::drop_na(tidyselect::all_of(impute_col))

  # initial rows for imputation - new data
  # remove rows that have data for the column we want to impute.
  # (no need to impute what we observed)
  data_new <- data_new[is.na(data_new[[impute_col]]), , drop = FALSE]

  # filter predictors, step 1:
  # drop any predictor that has only missing values in reference data
  impute_prds_keep <- purrr::map_lgl(
    .x = impute_prds,
    .f = ~any(!is.na(data_ref[[.x]]))
  )

  impute_prds <- impute_prds[impute_prds_keep]

  # this is only needed if new_data was supplied
  if(new_data_supplied){
    # filter predictors, step 2:
    # drop any predictor that has only missing values in new data.
    impute_prds_keep <- purrr::map_lgl(
      .x = impute_prds,
      .f = ~any(!is.na(data_new[[.x]]))
    )
    impute_prds <- impute_prds[impute_prds_keep]
  }

  # step 3: filter down to npred predictors (not implemented yet)

  # these are the observed values of the column we want to impute.
  # when we have nearest neighbor indices, we will aggregate
  # values from this vector according to which indices are those
  # belongining to the k nearest neighbors
  impute_vals <- purrr::pluck(data_ref, impute_col)

  # select the predictors that were retained after steps 1 and 2 above
  data_ref %<>% dplyr::select(tidyselect::all_of(impute_prds))
  data_new %<>% dplyr::select(tidyselect::all_of(impute_prds))

  # if the new data have rows that are contain nothing but missing
  # data, then there is nothing we can do to impute.
  rows_all_na <- apply(data_new, 1, function(x) all(is.na(x)))

  if(any(rows_all_na)){
    stop("some rows are missing data for all predictors", call. = FALSE)
  }

  # this is needed when nrow(data_ref) is small. Should there be a warning?
  gwr_n <- min( max(k_neighbors), nrow(data_ref) )

  # identify nearest neighbors via gower's distance
  # keep gower_topn quiet - it may send warnings about
  # skipping variables with zero or non-finite ranges.
  gwr_topn <- suppressWarnings(
    gower::gower_topn(
      x = data_new,
      y = data_ref,
      n = gwr_n,
      eps = epsilon,
      nthread = nthread
    )$index
  )

  # this vector contains the indices of data that need to be
  # imputed in the given column.
  impute_index <- which(is.na(data_out[[impute_col]]))

  # use the gower indices of nearest neighbors to fill in
  # values that belong to the nearest neighbors. Start with
  # a blank matrix.
  gwr_vals <- matrix(
    data = NA,
    nrow = nrow(gwr_topn),
    ncol = ncol(gwr_topn)
  )

  # fill in each column of the matrix by grabbing the impute_vals
  # vector at the ith column of index values
  for(i in seq(nrow(gwr_vals))){
    gwr_vals[i, ] <- impute_vals[ gwr_topn[i, ] ]
  }

  impute_output <- purrr::map(
    .x = k_neighbors,
    .f = aggr_neighbors,
    mtx = gwr_vals,
    fun = aggregate_function
  )

  # factor values are converted to integers when they get passed to
  # gwr_vals - convert them back to the given levels of impute_vals.
  if(var_type == 'factor'){

    labels <- levels(impute_vals)
    names(labels) <- seq(length(labels))

    impute_output <- purrr::map(
      .x = impute_output,
      .f = ~ dplyr::recode(.x, !!!labels)
    )

  }

  purrr::map(
    .x = impute_output,
    .f = ~ {
      .output <- data_out[[impute_col]]
      .output[impute_index] <- .x
      .output
    }
  )

}

aggr_neighbors <- function(k_neighbors, mtx, fun){

  ### rationale for the warning below:
  # sometimes you can't get enough neighbors because the data
  # have too few observations. If this is the case, we'd rather
  # not throw a hard error and stop the entire imputation procedure,
  # but we do need to say that the number of neighbors requested
  # exceeded the number of observed cases.

  # truncate the number of neighbors if needed
  if(nrow(mtx) < k_neighbors){

    warning("neighbor count (", k_neighbors, ") exceeds the number",
      " of observed data points (", nrow(mtx), ").",
      "\nimputed values will only use observed data.",
      call. = FALSE
    )

    k_neighbors <- nrow(mtx)

  }

  apply(mtx[1:k_neighbors, , drop = FALSE], 2, fun)


}

# Exported functions ------------------------------------------------------

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
#'   will contain an imputed version of `data_new`. If not supplied,
#'   the output will contain an imputed version of `data_ref`.
#'
#' @param cols a character vector containing column names that should be
#'   imputed.
#'
#' @param k a numeric vector indicating how many neighbors should be used
#'   to impute missing values.
#'
#' @param aggregate_neighbors a logical value. If `TRUE`, then neighbors
#'   will be aggregated to generate imputations. If `FALSE`, then one
#'   neighbor will be sampled at random to generate a missing value. Using
#'   `aggregate_neighbors = FALSE` can be helpful if you are conducting
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
#' @param verbose a numeric value indicating how much output should be
#'   printed to the console.
#'
#'   - `0`: nothing is printed
#'   - `1`: one message is printed for each column being imputed
#'
#' @return a list of imputed datasets the same length as `k`.
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
#' trn_imputes <- impute_knn(data_ref = trn, k = 1:5)
#' tst_imputes <- impute_knn(data_ref = trn, data_new = tst, k = 1:5)
#'
impute_knn <- function(
  data_ref,
  data_new = NULL,
  cols = dplyr::everything(),
  k_neighbors = 10,
  aggregate_neighbors = TRUE,
  fun_aggr_ctns = NULL,
  fun_aggr_intg = NULL,
  fun_aggr_catg = NULL,
  nthread = getOption("gd_num_thread"),
  epsilon = 1e-08,
  verbose = 0
) {

  # .cols = columns to be imputed
  .cols <- names(data_ref) %>%
    tidyselect::vars_select(!!rlang::enquo(cols))

  # Safety checks start:

  # data_ref should have >0 observed values in each row/column
  check_data_ref(data_ref, cols = .cols)

  # variable types should be...
  vt <- c('numeric', 'integer', 'logical', 'character', 'factor')
  check_var_types(data_ref, valid_types = vt)

  # if new data are supplied,
  # it should have exactly the Same names and types as reference data
  if(!is.null(data_new)){
    check_data_new_names(data_ref, data_new)
    check_data_new_types(data_ref, data_new)
  }

  # Safety checks done

  # columns that are not being imputed
  .cols_left_out <- setdiff(names(data_ref), .cols)

  imputed_values <- purrr::map(
    .x = .cols,
    .f = ~ impute_knn_col(
      data_ref   = data_ref,
      data_new   = data_new,
      impute_col = .x,
      k          = k_neighbors,
      aggregate_neighbors = aggregate_neighbors,
      fun_aggr_ctns = fun_aggr_ctns,
      fun_aggr_intg = fun_aggr_intg,
      fun_aggr_catg = fun_aggr_catg,
      nthread = nthread,
      epsilon = epsilon,
      verbose = verbose
    )
  )

  impute_dfs <- imputed_values %>%
    purrr::map_dfr(
      .f = tibble::enframe,
      name = NULL,
      .id = 'variable'
    ) %>%
    tidyr::pivot_wider(
      names_from = variable,
      values_from = value,
      values_fn = list(value = list)
    ) %>%
    tidyr::unnest(cols = names(.)) %>%
    base::apply(1, dplyr::bind_cols)

  if(!purrr::is_empty(.cols_left_out)){

    cols_to_add <- if(is.null(data_new))
      data_ref[, .cols_left_out, drop = FALSE]
    else
      data_new[, .cols_left_out, drop = FALSE]

    impute_dfs <- purrr::map(
      .x = impute_dfs,
      .f = ~ dplyr::bind_cols(.x, cols_to_add) %>%
        dplyr::select(tidyselect::all_of(names(data_ref)))
    )

  }

  impute_dfs

}
