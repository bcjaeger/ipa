
#' Soft imputation
#'
#' The `softImpute` algorithm is used to impute missing values.
#'  For more details, see [softImpute][softImpute::softImpute()]
#'
#' @inheritParams impute_nbrs
#'
#' @param rank_max_init an integer value that restricts the rank of the
#'   solution for the first `softImpute` fit. Sequential fits may have
#'   higher rank depending upon `rank_max_ovrl`, `rank_stp_size`, and `grid`.
#'
#' @param rank_max_ovrl an integer value that restricts the rank of the
#'   solution for all `softImpute` fits.
#'
#' @param rank_stp_size an integer value that indicates how much the maximum
#'   rank of `softImpute` fits should increase between iterations.
#'
#' @param lambda nuclear-norm regularization parameter. If `lambda = 0`,
#'   the algorithm reverts to "hardImpute", for which convergence is typically
#'   slower, and to local minimum. Ideally lambda should be chosen so that
#'   the solution reached has rank slightly less than rank.max. See also
#'   `lambda0()` for computing the smallest `lambda` with a zero solution.
#'
#' @param grid a logical value. If `TRUE`, all combinations of rank and
#'   lambda are used to fit `softImpute` models. If `FALSE`, then one
#'   fit is supplied for each value of `lambda`, and increasing maximum
#'   ranks are paired with decreasing values of `lambda`.
#'
#' @param restore_data a logical value. If `TRUE`, the variable types
#'   of the imputed values will match those of the original data. If `FALSE`,
#'   the imputed values are returned in a one-hot encoded format.
#'
#' @param verbose an integer value of 0, 1, or 2. If `verbose = 0`, nothing
#'  is printed. If `verbose = 1`, messages are printed to the console showing
#'  what general steps are being taken in the imputation process. If
#'  `verbose = 2`, all relevant information on convergence is printed in
#'  addition to general messages.
#'
#' @param bs a logical value. If `TRUE`, [softImpute::biScale()] is applied
#'   to `data_ref` or `rbind(data_ref, data_new)` prior to fitting `softImpute`
#'   models.
#'
#' @param bs_maxit an integer indicating the maximum number of iterations
#'   for the `biScale` algorithm.
#'
#' @param bs_thresh convergence threshold for the `biScale` algorithm.
#'
#' @param bs_row.center a logical value. If `TRUE`, row centering
#'   will be performed. If `FALSE` (default), then nothing is done.
#'
#' @param bs_col.center a logical value. If `TRUE` (default), column centering
#'   will be performed. If `FALSE`, then nothing is done.
#'
#' @param bs_row.scale a logical value. If `TRUE`, row scaling
#'   will be performed. If `FALSE` (default), then nothing is done.
#'
#' @param bs_col.scale a logical value. If `TRUE` (default), column scaling
#'   will be performed. If `FALSE`, then nothing is done.
#'
#' @param si_type two algorithms are implemented, type="svd" or the default
#'  type="als". The "svd" algorithm repeatedly computes the svd of the
#'  completed matrix, and soft thresholds its singular values. Each new
#'  soft-thresholded svd is used to re-impute the missing entries. For
#'  large matrices of class "Incomplete", the svd is achieved by an
#'  efficient form of alternating orthogonal ridge regression. The
#'  "als" algorithm uses this same alternating ridge regression, but
#'  updates the imputation at each step, leading to quite substantial
#'  speedups in some cases. The "als" approach does not currently
#'  have the same theoretical convergence guarantees as the
#'  "svd" approach.
#'
#' @param si_final.svd only applicable to type="als". The alternating
#'  ridge-regressions do not lead to exact zeros. With the default
#'  final.svd=TRUE, at the final iteration, a one step unregularized
#'  iteration is performed, followed by soft-thresholding of the
#'  singular values, leading to hard zeros.

#' @param si_thresh convergence threshold for the `softImpute` algorithm,
#'  measured as the relative change in the Frobenius norm between two
#'  successive estimates.
#'
#' @param si_maxit maximum number of iterations for the `softImpute`
#'   algorithm.
#'
#' @param si_final.svd only applicable to `si_type = "als"`. The alternating
#'   ridge-regressions do not lead to exact zeros. With the default
#'   `final.svd = TRUE`, at the final iteration, a one step unregularized
#'   iteration is performed, followed by soft-thresholding of the
#'   singular values, leading to hard zeros.
#'
#' @return a data frame with fitting parameters and imputed values.
#'
#' @details
#'
#' **Multiple imputation**: The number of imputations returned depends on
#'   `rank_max_init`, `rank_max_ovrl`, `rank_stp_size`, `lambda`, and `grid`.
#'    If `grid` is `FALSE`, then there will be `length(lambda)` imputed value
#'    sets in the returned output, and they will be based on fitted
#'    `softImpute` models with increasing maximum ranks. Generally, these ranks
#'    are `seq(rank_max_init, rank_max_ovrl, by = rank_stp_size)`, but will
#'   be automatically adjusted to have consistency with (1) `lambda` and
#'   (2) the maximum allowed rank for `data_ref` as needed. If `grid` is
#'   `TRUE`, then every combination of `lambda` and the rank sequence
#'   will be fitted and the output will contain one set of imputed values
#'   for each combination.
#'
#' **Rank inputs**: If rank is sufficiently large, and with `si_type="svd"`,
#'   the `softImpute` algorithm solves the nuclear-norm convex
#'   matrix-completion problem (see Reference 1). In this case the number
#'   of nonzero singular values returned will be less than or equal to
#'   the maximum rank. If smaller ranks are used, the solution is not
#'   guaranteed to solve the problem, although still results in good local
#'   minima. The rank of a `softImpute` fit should not exceed
#'   `min(dim(data_ref) - 1`.
#'
#' **biScale** The [softImpute::biScale()] function is more flexible than
#'   the current function indicates. Specifically, `biScale` allows users
#'   to supply vectors to its row/column centering/scaling inputs that will
#'   in turn be used to center/scale the corresponding rows/columns.
#'   `impute_soft()` is more strict and does not offer this option.
#'   Also, `impute_soft()` uses different default values to increase the
#'   likelihood of the `biScale` algorithm converging quickly.
#'
#' @references
#'
#' 1. Rahul Mazumder, Trevor Hastie and Rob Tibshirani (2010) Spectral
#' Regularization Algorithms for Learning Large Incomplete Matrices,
#' http://www.stanford.edu/~hastie/Papers/mazumder10a.pdf,
#' Journal of Machine Learning Research 11 (2010) 2287-2322
#'
#' @export
#'
impute_soft <- function(
  data_ref,
  data_new = NULL,
  cols = dplyr::everything(),
  rank_max_ovrl = min(dim(data_ref)-1),
  rank_max_init = min(2, rank_max_ovrl),
  rank_stp_size = 1,
  lambda = seq(rank_max_ovrl * 0.60, 1, length.out = 10),
  grid = FALSE,
  restore_data = TRUE,
  verbose = 1,
  bs = TRUE,
  bs_maxit = 20,
  bs_thresh = 1e-09,
  bs_row.center = FALSE,
  bs_col.center = TRUE,
  bs_row.scale = FALSE,
  bs_col.scale = TRUE,
  si_type = "als",
  si_thresh = 1e-05,
  si_maxit = 100,
  si_final.svd = TRUE
){

  # keep_cols = columns to be imputed
  keep_cols <- names(data_ref) %>%
    tidyselect::vars_select(!!rlang::enquo(cols))

  if(length(keep_cols) == 1) stop("1 column was selected (",
    keep_cols, ") but 2+ are needed", call. = FALSE)

  # if a subset of columns were selected, the maximum ranks
  # and lambda sequence need to be reset:
  if(length(keep_cols) < ncol(data_ref)){

    rank_max_ovrl = min(c(nrow(data_ref), length(keep_cols))-1)
    rank_max_init = min(2, rank_max_ovrl)

  }

  # Safety checks start:

  check_int(rank_max_init, label = 'initial max rank (rank_max_init)')
  check_int(rank_stp_size, label = 'rank step size (rank_stp_size)')
  check_int(rank_max_ovrl, label = 'overall max rank')

  check_l1_warn(grid, label = 'grid')
  check_bool(grid, label = 'grid')

  check_int(verbose, label = 'verbosity')
  check_min_lax(verbose, label = 'verbosity', value = 0)
  check_max_lax(verbose, label = 'verbosity', value = 2)

  check_min_lax(rank_max_init, value = 1,
    label = 'initial max rank (rank_max_init)')

  check_max_lax(rank_max_init, value = min(dim(data_ref) - 1),
    label = 'initial max rank (rank_max_init)')

  check_min_lax(rank_max_ovrl, value = 1,
    label = 'overall max rank (rank_max_ovrl)')

  check_max_lax(rank_max_ovrl, value = min(dim(data_ref) - 1),
    label = 'overall max rank (rank_max_ovrl)')

  check_min_lax(rank_stp_size, value = 1,
    label = 'rank step size (rank_stp_size)')

  check_min_lax(lambda, label = 'lambda', value = 0)

  check_bool(bs, label = 'biScale indicator (bs)')
  check_l1_stop(bs, label = 'biScale indicator (bs)')

  check_int(bs_maxit, label = 'maximum biScale iterations (bs_maxit)')
  check_min_lax(bs_maxit, value = 0,
    label = 'maximum biScale iterations (bs_maxit)')

  check_min_lax(bs_thresh, value = 0,
    label = 'biScale convergence threshold (bs_thresh)')

  check_min_lax(bs_thresh, value = 0,
    label = 'biScale convergence threshold (bs_thresh)')

  check_bool(bs_row.scale,
    label = 'biScale row scaling indicator (bs_row.scale)')
  check_bool(bs_col.scale,
    label = 'biScale column scaling indicator (bs_col.scale)')
  check_bool(bs_row.center,
    label = 'biScale row centering indicator (bs_row.center)')
  check_bool(bs_col.center,
    label = 'biScale column centering indicator (bs_col.center)')

  check_l1_stop(bs_row.scale,
    label = 'biScale row scaling indicator (bs_row.scale)')
  check_l1_stop(bs_col.scale,
    label = 'biScale column scaling indicator (bs_col.scale)')
  check_l1_stop(bs_row.center,
    label = 'biScale row centering indicator (bs_row.center)')
  check_l1_stop(bs_col.center,
    label = 'biScale column centering indicator (bs_col.center)')

  check_chr(si_type, options = c('als', 'svd'),
    label = 'softImpute type (si_type)')

  check_min_lax(si_thresh, value = 0,
    label = 'softImpute convergence threshold (si_thresh)')

  bs_trace <- si_trace <- verbose > 1

  # if data_ref is given and nothing else
  # --> create fits for data_ref, return fits + imputed data refs
  # if data_ref/data_new are given, but no fits
  # --> create fits for rbind(data_ref, data_new), return imputed data_news
  # if data_ref/data_new + fits are given,
  # --> same as above but use warm starts

  # convert data frames into data.table objects if needed
  if(!is.data.table(data_ref))
    DT_ref <- as.data.table(data_ref)[, ..keep_cols]
  else
    DT_ref <- data_ref[, ..keep_cols]

  # convert characters to factors
  # modifying in place rather than copying data
  # the code is less readable but more performant
  if(any(sapply(DT_ref, is.character))){
    chr_cols <- names(DT_ref)[sapply(DT_ref, is.character)]
    DT_ref[, (chr_cols) := lapply(.SD, as.factor), .SDcols = chr_cols]
  }

  # initialize a null DT_new object in case there isn't any new data
  DT_new <- NULL

  # repeat the code above for the testing data if it exists.
  if(!is.null(data_new)){

    # convert data frames into data.table objects if needed

    if(!is.data.table(data_new))
      DT_new <- as.data.table(data_new)[, ..keep_cols]
    else
      DT_new <- data_new[, ..keep_cols]


    if(any(sapply(DT_new, is.character))){

      chr_cols <- names(DT_new)[sapply(DT_new, is.character)]
      DT_new[, (chr_cols) := lapply(.SD, as.factor), .SDcols = chr_cols]

    }


  }

  # variable types should be...
  check_var_types(DT_ref, valid_types = c('numeric', 'integer', 'factor'))

  # if new data are supplied,
  # it should have exactly the same names and types as reference data
  if(!is.null(data_new)){
    check_data_new_names(DT_ref, DT_new)
    check_data_new_types(DT_ref, DT_new)
  }

  # bind data into one bundle
  # (this is the only way for si to impute new data)
  # (this also does nothing if DT_new is NULL)
  data_all <- rbindlist(list(DT_ref, DT_new))

  impute_indx <- mindx(data_all, drop_empty = TRUE)

  if(is_empty(impute_indx)){
    warning("There are no missing values to impute",
      call. = FALSE)
    # TODO: make this output consistent with the normal output
    # (for grid = TRUE)
    output <- data.table(
      impute   = seq(length(lambda)),
      lambda   = lambda,
      rank_max = NA,
      rank_fit = NA,
      fit      = NA,
      impute_vals = list(NULL)
    )

    return(output)
  }

  # check for missing rows/columns
  check_missingness(impute_indx, N = nrow(data_all),
    P = ncol(data_all), label = 'stacked data')
  # drop empty cols from miss_indx
  impute_indx <- drop_empty(impute_indx)


  # names with . in front indicate one-hot encoded data
  # data_all and .data_all are both needed - don't try to optimize
  .data_all <- one_hot(data_all)

  .impute_indx <- mindx(.data_all, drop_empty = TRUE)

  if(bs){

    if(verbose > 0) message("Applying biScale() to data")

    # safety checks for biscale
    # if you scale columns, each column needs >= 2 unique values.
    if(bs_col.scale){

      cnst_cols <- .data_all %>%
        apply(2, function(x) length(unique(na.omit(x)))==1)

      if(any(cnst_cols)){

        cnst_cols <- names(which(cnst_cols))

        stop('cannot compute standard deviation (i.e., column scale)',
          ' because some columns are constant: ', list_things(cnst_cols),
          call. = FALSE)

      }

    }

    if(bs_row.scale){

      cnst_rows <- .data_all %>%
        apply(1, function(x) length(unique(na.omit(x))) == 1)

      if(any(cnst_rows)){

        cnst_rows <- which(cnst_rows)

        stop('cannot compute standard deviation (i.e., row scale)',
          ' because some rows are constant: ', list_things(cnst_rows),
          call. = FALSE)

      }

    }

    # this converts .data_all into a matrix
    # and adds scaling attributes to it
    .data_all <- .data_all %>%
      softImpute::biScale(
        maxit      = bs_maxit,
        thresh     = bs_thresh,
        row.center = bs_row.center,
        row.scale  = bs_row.scale,
        col.center = bs_col.center,
        col.scale  = bs_col.scale,
        trace      = bs_trace
      )

  } else {

    .data_all <- as.matrix(.data_all)

  }

  .args_softimp = list(
    type       = si_type,
    thresh     = si_thresh,
    maxit      = si_maxit,
    trace.it   = si_trace,
    final.svd  = si_final.svd
  )

  .soft_fun <- if(grid) .soft_fit_grid else .soft_fit

  imputes <- .soft_fun(
    data            = .data_all,
    verbose         = verbose,
    rank_max_init   = rank_max_init,
    rank_max_ovrl   = rank_max_ovrl,
    rank_stp_size   = rank_stp_size,
    lambda_sequence = lambda,
    args_softimp    = .args_softimp
  )

  if(verbose > 0){

    text <- 'obtaining imputed values'

    if(restore_data) text <- paste(text,
      'and restoring original types')

    message(text)

  }

  # TODO: make this faster
  impute_vals <- imputes$fit %>%
    # converting the soft impute fits into a list of numeric values
    # corresponding to the indices that need to be imputed for each
    # one-hot encoded column.
    purrr::map(.f = get_softImpute_values,
      miss_row    = .impute_indx,
      data        = .data_all)


  if(restore_data){
    # converting the imputed columns back into the format given in data.
    # this should leave us with a list that can be directly plugged in.
    impute_vals <- purrr::map(
      .x = impute_vals,
      .f = restore_vectypes,
      data = data_all,
      impute_indx = impute_indx,
      fctr_info = get_factor_info(data_all)
    )

  }

  # if DT_new are supplied, only DT_new should be imputed.
  if(!is.null(data_new)){

    # if we aren't restoring data types, then the one hot impute index
    # can take over here.
    if(!restore_data) impute_indx <- .impute_indx

    # the indices in impute_indx should be kept only if they
    # index something in DT_new, i.e., only if they are
    # greater than the number of rows in DT_ref
    impute_indx <- purrr::map(impute_indx, ~.x > nrow(DT_ref))
    # re-order names of impute_indx to match impute vals
    impute_indx <- impute_indx[names(impute_vals[[1]])]

    impute_vals <- purrr::map(
      .x = impute_vals,
      .f = function(ivals){
        purrr::map2(ivals, impute_indx, ~.x[.y]) %>%
          # missing data in training data might not be missing in testing
          # data. If this is the case, impute_vals should not include
          # those columns, which will be empty at this point. Drop em.
          drop_empty()
      }
    )

  }


  set(imputes, j = 'imputed_values', value = impute_vals)

  imputes

}


restore_vectypes <- function(impute_vals, data, impute_indx, fctr_info){

  fctr_imputes <- fctr_info$keys %>%
    purrr::map(~do.call(cbind, impute_vals[.x])) %>%
    drop_empty() %>%
    purrr::map(apply, 1, which.max)

  if(!is_empty(fctr_imputes)){

    for(f in names(fctr_imputes)){

      fctr_imputes[[f]] <- factor(
        fctr_imputes[[f]],
        levels = 1:length(fctr_info$lvls[[f]]),
        labels = fctr_info$lvls[[f]]
      )

    }

    impute_vals[unlist(fctr_info$keys)] <- NULL

  }

  # these are all either double or integer values
  # imputed values for integers values are coerced
  # and truncated to the observed min/max
  for(col in names(impute_vals)){

    if (is.integer(data[[col]])) {
      impute_vals[[col]] <- as.integer(round(impute_vals[[col]]))
        # this would truncate to observed range,
        # but I am not sure that is a great idea.
        # pmin(max(data[[col]], na.rm = TRUE)) %>%
        # pmax(min(data[[col]], na.rm = TRUE))
    }


  }

  for(col in names(fctr_imputes)){

    impute_vals[[col]] <- fctr_imputes[[col]]

  }

  impute_vals

}

.soft_fit_grid <- function(
  data,
  verbose,
  rank_max_init,
  rank_max_ovrl,
  rank_stp_size,
  lambda_sequence,
  args_softimp
){

  rank_sequence <- seq(rank_max_init, rank_max_ovrl, by = rank_stp_size)

  n_fits <- length(lambda_sequence) * length(rank_sequence)

  # Containers for results
  # These get updated in the for-loop below
  fits <- vector(mode = 'list', length = n_fits)
  warm <- NULL
  indx <- 1

  if(verbose > 0) message("Fitting soft-impute models")

  for(i in seq_along(rank_sequence)){

    for(j in seq_along(lambda_sequence)){

      fits[[indx]] <- softImpute::softImpute(
        x          = data,
        rank.max   = rank_sequence[i],
        lambda     = lambda_sequence[j],
        type       = args_softimp$type,
        thresh     = args_softimp$thresh,
        maxit      = args_softimp$maxit,
        trace.it   = args_softimp$trace.it,
        final.svd  = args_softimp$final.svd,
        warm.start = warm
      )

      # Determine the rank of the fit, which may or may not
      # be as high as the maximum rank.
      rank_fit <- sum(round(fits[[indx]]$d, 4) > 0)

      attr(fits[[indx]], 'rank')     <- as.integer(rank_fit)
      attr(fits[[indx]], 'rank_max') <- as.integer(rank_sequence[i])

      if(verbose > 0){

        message(
          glue::glue(
            "fit {indx} of {n_fits}: \\
            lambda = {format(round(lambda_sequence[j], 3),nsmall=3)}, \\
            rank.max = {rank_sequence[i]} \\
            rank.fit = {rank_fit}"
          )
        )

      }

      # update fitting parameters
      rank_max <- min(rank_fit + rank_stp_size, rank_max_ovrl)

      warm <- fits[[indx]]

      indx <- indx + 1

    }

    warm <- fits[[indx - length(lambda_sequence)]]

  }

  expand.grid(
    lambda = lambda_sequence,
    rank_max = rank_sequence
  ) %>%
    setDT() %>%
    set(j = 'impute', value = seq(n_fits)) %>%
    set(j = 'rank_fit', value = purrr::map_int(fits,~attr(.x,'rank'))) %>%
    set(j = 'fit', value = fits) %>%
    setcolorder(
      neworder = c('impute', 'lambda', 'rank_max', 'rank_fit', 'fit')
    )

}

.soft_fit <- function(
  data,
  verbose,
  rank_max_init,
  rank_max_ovrl,
  rank_stp_size,
  lambda_sequence,
  args_softimp
){

  n_fits <- length(lambda_sequence)

  # Containers for results
  # These get updated in the for-loop below
  fits <- vector(mode = 'list', length = n_fits)
  rank_max <- rank_max_init
  warm <- NULL

  if(verbose > 0) message("Fitting soft-impute models")

  for( i in seq(n_fits) ){

    # create softimpute fit
    fits[[i]] <- softImpute::softImpute(
      x          = data,
      rank.max   = rank_max,
      lambda     = lambda_sequence[i],
      type       = args_softimp$type,
      thresh     = args_softimp$thresh,
      maxit      = args_softimp$maxit,
      trace.it   = args_softimp$trace.it,
      final.svd  = args_softimp$final.svd,
      warm.start = warm
    )

    # Determine the rank of the fit, which may or may not
    # be as high as the maximum rank.
    rank_fit <- sum(round(fits[[i]]$d, 4) > 0)

    attr(fits[[i]], 'rank')     <- as.integer(rank_fit)
    attr(fits[[i]], 'rank_max') <- as.integer(rank_max)

    if(verbose > 0){

      message(
        glue::glue(
          "fit {i} of {n_fits}: \\
            lambda = {format(round(lambda_sequence[i], 3),nsmall=3)}, \\
            rank.max = {rank_max} \\
            rank.fit = {rank_fit}"
        )
      )

    }

    # update fitting parameters
    rank_max <- min(rank_fit + rank_stp_size, rank_max_ovrl)
    warm <- fits[[i]]


  }

  data.table(
    impute   = seq(n_fits),
    lambda   = lambda_sequence,
    rank_max = purrr::map_int(fits, ~attr(.x, 'rank_max')),
    rank_fit = purrr::map_int(fits, ~attr(.x, 'rank')),
    fit      = fits
  )

}


get_softImpute_values <- function(fit, miss_row, data){

  miss_col <- purrr::map(names(miss_row),
    .f = match, table = colnames(data))

  for(i in seq_along(miss_col)){
    miss_col[[i]] <- rep(miss_col[[i]], length(miss_row[[i]]))
  }

  purrr::map2(
    .x = miss_row,
    .y = miss_col,
    .f = ~ softImpute::impute(object = fit, i = .x, j = .y) %>%
      purrr::set_names(NULL)
  )

}
