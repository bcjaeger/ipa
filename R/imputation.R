

#' Make a soft malt.
#'
#' @description The `softImpute` algorithm is used to impute
#'   missing values with this `brew`.
#'
#'   For more details on this strategy to handle missing values,
#'   please see [softImpute][softImpute::softImpute()].
#'
#' @section Overview:
#'   Brewing your own IPA is a lot like handling missing
#'   data. When a brew is initiated, the first step is to determine
#'   the type of wheat and prepare it for mashing. For the `ipa`
#'   package, this amounts to determining the type of imputation
#'   method and preparing it for use. The `brew` object is
#'   prepared for mashing (the next step) by identifying a
#'   training data set and the outcome(s) of the analysis.
#'
#' @param training_data data that will be used to create imputation models
#'
#' @param outcome column name(s) of outcomes in `training_data`.
#'
#' @export
#'
soft_malt <- function(training_data, outcome){

  outcome <- vars_select(names(training_data), !!enquo(outcome)) %>%
    set_names(NULL)

  new_data <- training_data

  new_data[, outcome] = NULL

  numeric_cols <- map_lgl(new_data, is.numeric)

  if(!all(numeric_cols)){

    stop(
      paste(
        "All columns should be numeric. Check the following:",
        list_things(names(numeric_cols)[!numeric_cols])
      )
    )

  }

  output <- structure(
    .Data = list(
      data = new_data,
      pars = list(max_rank = min(dim(new_data)) - 1),
      wort = NULL
    ),
    class = 'soft_brew',
    outcome_lab = outcome,
    outcome_trn = training_data[, outcome, drop = FALSE],
    outcome_tst = NULL,
    malted = TRUE,
    mashed = FALSE,
    boiled = FALSE,
    fermented = FALSE
  )

  output

}

#' Make a neighborhood malt.
#'
#' @inheritSection soft_malt Overview
#' @inheritParams soft_malt
#'
#' @export
#'
#'
nbrs_malt <- function(training_data, outcome){

  outcome <- vars_select(names(training_data), !!enquo(outcome)) %>%
    set_names(NULL)

  new_data <- training_data

  new_data[, outcome] = NULL

  max_nbrs <- min(map_int(new_data, ~sum(complete.cases(.x))))

  output <- structure(
    .Data = list(
      data = new_data,
      pars = list(max_nbrs = max_nbrs),
      wort = NULL
    ),
    class = 'neighbors_brew',
    outcome_lab = outcome,
    outcome_trn = training_data[, outcome, drop = FALSE],
    outcome_tst = NULL,
    malted = TRUE,
    mashed = FALSE,
    boiled = FALSE,
    fermented = FALSE
  )

  output

}


#' Mash a soft mash
#'
#' @description The `softImpute` algorithm has a number of
#'   inputs that affect the imputation procedure. Parameters
#'   set in this step of the `brew` process include the
#'   progression of the maximum rank of `softImpute` solutions,
#'   whether or not to apply the `biScale` function (see notes),
#'   and additional parameters that give the user some control
#'   over `lambda`. See notes for links to the original
#'   `softImpute` functions.
#'
#' @section Overview:
#'   After malting a `brew`, the next step is to pair the
#'   malted wheat with hot water and mash it. In the `ipa` process,
#'   mashing is where an imputation method is paired with relevant
#'   hyper-parameters.
#'
#' @param malt a `soft_brew` object that has been malted
#'   (see [soft_malt]).
#'
#' @param n_impute An integer indicating the number of imputed
#'   datasets to create.
#'
#' @param step_size An integer indicating the number by which to increase
#'   the maximum rank of the softImpute solution after each iteration.
#'
#' @param scale_data `TRUE` or `FALSE`. If `TRUE`, the
#'   `biScale` algorithm will be applied to the training
#'   data prior to applying the `softImpute` algorithm. It
#'   is recommended that this argument be set to `TRUE`
#'
#' @param scale_iter An integer indicating the maximum number
#'   of iterations that will be applied in the `biScale`
#'   algorithm. It is recommended that this argument be managed
#'   to ensure convergence.
#'
#' @param scale_lambda A positive double less than 1. This constant
#'   scales the maximum value of `lambda` used to fit the
#'   `softImpute` function. Larger values of `lambda`
#'   correspond to lower maximum rank solutions.
#'
#' @param lambda_sequence A sequence of `lambda` values. If
#'   specified, these `lambda` values will be used instead of
#'   the `lambda` values that `soft_mash()` automatically identifies.
#'
#' @note see [softimpute][softImpute::softImpute()] for a more
#'   descriptive summary of the `softImpute` algorithm. See
#'   [biScale][softImpute::biScale()] for a more descriptive
#'   summary of the `biScale` algorithm.
#'
#' @author Trevor Hastie, Rahul Mazumder
#'
#' @references Rahul Mazumder, Trevor Hastie and Rob Tibshirani (2010)
#'   Spectral Regularization Algorithms for Learning Large Incomplete
#'   Matrices, http://www.stanford.edu/~hastie/Papers/mazumder10a.pdf
#'   *Journal of Machine Learning Research* 11 (2010) 2287-2322
#' @export

soft_mash <- function(
  malt,
  n_impute = 10,
  step_size = 1,
  scale_data = TRUE,
  scale_iter = 20,
  scale_lambda = 0.95,
  lambda_sequence = NULL
){

  if(is.null(attr(malt, 'malted'))) stop(
    "the brew has not been mashed! Try using soft_malt() before boiling",
    call. = FALSE
  )

  check_pos_int(n_impute, label = 'number of imputations')
  check_pos_int(step_size, label = 'step size')
  check_pos_int(scale_iter, label = 'number of iterations for bi-scale')

  check_step_size(
    step_size = step_size,
    n_impute =  n_impute,
    max_rank =  malt$pars$max_rank
  )

  if(!is.logical(scale_data))
    stop('scale_data should be logical', call. = FALSE)

  if(scale_lambda > 1 || scale_lambda < 0)
    stop('scale_lambda should be in [0,1]', call.=FALSE)

  malt$pars <- list(
    max_rank = malt$pars$max_rank,
    n_impute = n_impute,
    step_size = step_size,
    scale_data = scale_data,
    scale_iter = scale_iter,
    scale_lambda = scale_lambda,
    lambda_sequence = lambda_sequence
  )

  attr(malt, 'mashed') <- TRUE

  malt

}

#' Make a neighborhood mash
#' @export
nbrs_mash <- function(
  malt,
  n_impute = 10,
  step_size = NULL,
  neighborhood = NULL
){

  check_malt(malt)

  check_pos_int(n_impute, label = 'number of imputations')

  step_null <- is.null(step_size)
  nbrs_null <- is.null(neighborhood)

  if(step_null && nbrs_null) stop(
    "step_size or neighborhood must be specified"
  )

  if(!step_null && !nbrs_null) stop(
    "only one of step_size or neighborhood should be specified"
  )

  if(nbrs_null){

    check_pos_int(step_size, label = 'step size')
    check_step_size(
      step_size = step_size,
      n_impute =  n_impute,
      max_rank =  malt$pars$max_nbrs
    )

    nb_agg <- 'summarize'
    nb_seq <- seq(1, n_impute * step_size, by = step_size)

  } else {

    nb_seq <- neighborhood

    if(length(neighborhood) == 1){

      check_pos_int(neighborhood, label = 'neighborhood size')
      nb_agg <- 'sample'
      nb_seq <- rep(nb_seq, n_impute)

    } else {

      nb_agg <- 'summarize'

      if(length(neighborhood) != n_impute) warning(
        "neighborhood should be the same length as n_impute. \n",
        "I will switch n_impute to be the same length as neighborhood."
      )

      n_impute <- length(neighborhood)

    }

  }

  malt$pars <- list(
    max_nbrs = malt$pars$max_nbrs,
    n_impute = n_impute,
    nbrs_seq = nb_seq,
    nbrs_agg = nb_agg
  )

  attr(malt, 'mashed') <- TRUE

  malt

}


#' Boil a brew
#'
#' @description When an IPA is boiled, hops and spices are added
#'   to the wort, creating flavors for enjoyment. For an `ipa` `brew`
#'   object, this step involves adding training data to the analysis
#'   and then creating imputation models based on that data.
#'
#' @param mash a `brew` object that has been mashed (see [soft_mash])
#'
#' @param verbose `TRUE` or `FALSE`. if `TRUE`, output will be
#'   printed to the console indicating the ranks of solutions found
#'   for the softImpute fits.
#'
#' @param ... arguments passed on to [softImpute][softImpute::softImpute()],
#'   [gowerD][gower::gower_dist()],
#'
#' @note For `soft_brew` objects, valid inputs for `...` are
#'  - `type`: two algorithms are implements, type="svd" or the default
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
#'  - `thresh`: convergence threshold, measured as the relative
#'  change in the Frobenius norm between two successive estimates.
#'
#'  - `maxit`: maximum number of iterations.
#'
#'  - `final.svd`: only applicable to type="als". The alternating
#'  ridge-regressions do not lead to exact zeros. With the default
#'  final.svd=TRUE, at the final iteration, a one step unregularized
#'  iteration is performed, followed by soft-thresholding of the
#'  singular values, leading to hard zeros.
#'
#'  For `neighbors_brew` objects, valid inputes for `...` are
#'  - `eps`:	(numeric; optional) Computed numbers (variable ranges) smaller
#'  than eps are treated as zero.
#'  - `weights`	(numeric; optional) A vector of weights of length
#'  equal to the number of predictor variables in the mash that defines
#'  the weight applied to each component of the gower distance.
#'
#' @export
boil <- function(mash, verbose=FALSE, ...){
  UseMethod('boil')
}

#' @describeIn boil boil a soft brew, valid parameters for `...` are
#'   `type`, `thresh`, `maxit`, `trace.it`, and `final.svd` (see notes).
#' @export
#'
boil.soft_brew <- function(mash, verbose=FALSE, ...){

  check_mash(mash)

  .dots <- list(...) %>%
    check_dots(
      valid_args = c(
        'type',
        'thresh',
        'maxit',
        'trace.it',
        'final.svd'
      )
    )

  lam0 <- lambda0(x = mash$data) * mash$pars$scale_lambda

  lamseq <- mash$pars$lambda_sequence %||%
    seq(lam0, 1, length = mash$pars$n_impute)

  fits = as.list(lamseq)
  ranks = as.integer(lamseq)
  rank.max = rank_max = mash$pars$max_rank
  warm = NULL

  .dots$x <- as.matrix(mash$data)

  if(mash$pars$scale_data){

    if(verbose){
      message("Applying biScale() to data")
    }

    .dots$x %<>% biScale(
      maxit = mash$pars$scale_iter,
      trace = verbose
    )

  }

  if(verbose) message(
    "Fitting soft-impute models"
  )

  for( i in seq_along(lamseq) ){

    .dots$lambda <- lamseq[i]
    .dots$rank.max = rank.max
    .dots$warm.start = warm

    fiti <- do.call(softImpute, args = .dots)

    attr(fiti, 'rank') <- ranks[i] <- sum(round(fiti$d, 4) > 0)
    rank.max = min(ranks[i] + mash$pars$step_size, rank_max)
    warm = fiti
    fits[[i]] = fiti

    if(verbose){

      print(
        glue(
          "fit {i} of {mash$pars$n_impute}: \\
          lambda = {format(round(lamseq[i], 3),nsmall=3)}, \\
          rank.max = {rank.max} \\
          rank.fit = {ranks[i]}"
        )
      )

    }

  }

  mash$wort <- enframe(fits, name = 'impute', value = 'fit') %>%
    mutate(lambda = lamseq, rank = ranks) %>%
    select(impute, lambda, rank, fit)

  attr(mash, 'boiled') = TRUE

  mash

}

#' @describeIn boil boil a soft brew, valid parameters for `...` are
#'   `eps` and `nthread` (see notes).
#' @export
#'
boil.neighbors_brew <- function(mash, verbose = FALSE, ...){

  check_mash(mash)

  .dots <- check_dots(list(...), valid_args = c('eps', 'nthread'))
  .dots$eps <- .dots$eps %||% 1e-08
  .dots$nthread <- .dots$nthread %||% getOption("gd_num_thread")
  .dots$verbose <- verbose
  .dots$ref_data <- mash$data
  .dots$n_impute <- mash$pars$n_impute
  .dots$neighbor_sequence <- mash$pars$nbrs_seq
  .dots$neighbor_aggr_fun <- mash$pars$nbrs_agg

  fits <- do.call(knn_work, args = .dots)

  mash$wort <- fits %>%
    enframe(name = 'impute', value = 'fit') %>%
    mutate(
      k_neighbors = mash$pars$nbrs_seq,
      aggr_fun = mash$pars$nbrs_agg
    ) %>%
    select(impute, k_neighbors, aggr_fun, fit)

  attr(mash, 'boiled') = TRUE

  mash

}

#' Ferment a brew
#'
#' @param brew a `brew` object that has been boiled (see [boil]).
#'
#' @param new_data testing data (i.e., data that will be used to
#'   test a predictive model) with missing values.
#'
#' @param col_name a character value indicating what the name of
#'   the imputed datasets will be in the `wort` (see notes).
#'
#' @param neighbors an integer value specifying the number of
#'   nearest neighbors to identify when imputing testing data.
#'   (see notes.)
#'
#' @param impute_index the index of the imputed training
#'   dataset that will be used to identify nearest neighbors
#'   in testing data. (see notes)
#'
#' @note That the `wort` is a component of `brew` objects that
#'   contains imputed datasets, models used to impute those datasets,
#'   and the corresponding hyper-parameters of those models.
#'
#'   The `neighbors` and `impute_index` arguments are
#'   necessary for imputation methods that are not compatible
#'   with imputation of testing data.
#'
#'   - [softimpute][softImpute::softImpute()] imputation method
#'   can only be applied too impute training data. So, when
#'   testing data need to be imputed, `ipa` will look for
#'   nearest neighbors of testing observations in the *imputed*
#'   training data. And because there may be more than one
#'   imputed training data set, `impute_index` allows the user
#'   to specify which one is used to find nearest neighbors
#'   for testing observations.
#'
#'   The `neighbors` and `impute_index` arguments are
#'   applicable (though not necessary) for k-nearest-neighbor
#'   imputation objects (i.e., `neighbors_brew` objects).
#'
#'
#'
#'
#' @export
ferment <- function(
  brew,
  new_data = NULL,
  col_name = 'testing',
  control = list()
) {
  UseMethod('ferment')
}

#' @describeIn ferment impute missing values for `soft_brew` objects.
#' @export
ferment.soft_brew <- function(
  brew,
  new_data = NULL,
  col_name = 'testing',
  control = list()
) {

  check_brew(brew, col_name)
  new_data_supplied <- !is.null(new_data)

  # Impute training data using softImpute
  .dots <- list(training = brew$data)

  outcome <- attr(brew, 'outcome_lab')

  if(new_data_supplied) {

    .dots[[col_name]] <- as_tibble(new_data)
    .names <- names(.dots[[col_name]])
    if(outcome %in% .names){
      attr(brew, 'outcome_tst') <- .dots[[col_name]][, outcome, drop = FALSE]
      .dots[[col_name]][, outcome] = NULL
    }

  }

  brew$wort %<>%
    mutate(
      training = map(
        .x = fit,
        .f = ~ .dots$training %>%
          softImpute::complete(.x, unscale = brew$pars$scale_data) %>%
          as_tibble()
      )
    )


  if(new_data_supplied){

    control <- check_dots(
      control,
      valid_args = c('neighbors', 'impute_index')
    )

    neighbors <- control$neighbors %||% 5
    impute_index <- control$impute_index %||% round(nrow(brew$wort)/2)

    nn_index <- gower_topn(
      x = .dots[[col_name]],
      y = brew$wort$training[[impute_index]],
      n = neighbors
    )$index

    brew$wort[[col_name]] <- map(
      .x = brew$wort$training,
      .f = ~ knn_brew(
        ref_data = .x,
        new_data = .dots[[col_name]],
        nn_index = nn_index
      )
    )
  }

  attr(brew, 'fermented') <- TRUE
  attr(brew, 'fermented_cols') <- names(.dots)

  brew

}

#' @describeIn ferment impute missing values for `neighbors_brew` objects.
#' @export
ferment.neighbors_brew <- function(
  brew,
  new_data = NULL,
  col_name = 'testing',
  control = list()
) {

  check_brew(brew, col_name)

  new_data_supplied <- !is.null(new_data)

  .dots <- list(training = brew$data)

  outcome <- attr(brew, 'outcome_lab')

  if(new_data_supplied) {

    .dots[[col_name]] <- as_tibble(new_data)
    .names <- names(.dots[[col_name]])
    if(outcome %in% .names){
      attr(brew, 'outcome_tst') <- .dots[[col_name]][, outcome, drop = FALSE]
      .dots[[col_name]][, outcome] = NULL
    }

  }

  brew$wort %<>% mutate(training = fit)

  .dots <- check_dots(control, valid_args = c('eps', 'nthread', 'verbose'))
  .dots$eps <- .dots$eps %||% 1e-08
  .dots$nthread <- .dots$nthread %||% getOption("gd_num_thread")
  .dots$verbose <- .dots$verbose %||% FALSE
  .dots$ref_data <- brew$data
  .dots$new_data <- new_data
  .dots$n_impute <- brew$pars$n_impute
  .dots$neighbor_sequence <- brew$pars$nbrs_seq
  .dots$neighbor_aggr_fun <- brew$pars$nbrs_agg

  brew$wort[[col_name]] <- do.call(knn_work, args = .dots)

  attr(brew, 'fermented') <- TRUE
  attr(brew, 'fermented_cols') <- names(.dots)

  brew

}

#' Bottle a brew
#'
#' @param brew a `brew` object that has been passed through all
#'   the brewing steps: [malt], [mash], [boil], and [ferment].
#'
#' @param flavor a character value indicating what composition
#'   the training and testing data will be returned as.
#'
#' @export
bottle <- function(brew, flavor = c('tibble', 'matrix')){

  UseMethod('bottle')

}

#' @describeIn bottle bottle up a soft brew
#' @export
bottle.soft_brew <- function(brew, flavor = c('tibble', 'matrix')){

  is_not_fermented <- !attr(brew, 'fermented')

  if(is_not_fermented) stop(
    "the brew has not been fermented! Try using ferment() before bottling",
    call. = FALSE
  )

  .fun <- switch (
    EXPR = flavor[1],
    'tibble' = as_tibble,
    'matrix' = as.matrix
  )

  .cols <- attr(brew, 'fermented_cols')

  brew$wort[[ .cols[1] ]] %<>% map(bind_cols, attr(brew, 'outcome_trn'))
  brew$wort[[ .cols[2] ]] %<>% map(bind_cols, attr(brew, 'outcome_tst'))
  brew$wort %<>% mutate_at(.cols, ~map(.x, .fun))
  brew$wort

}

#' mode estimation
#'
#' (copied from recipes)
#'
mode_est <- function (x)
{
  if (!is.character(x) & !is.factor(x))
    stop(
      "The data should be character or factor to compute the mode.",
      call. = FALSE
    )

  tab <- table(x)
  modes <- names(tab)[tab == max(tab)]
  sample(modes, size = 1)

}

nn_pred <- function(index, dat, k, random = FALSE) {

  dat <- dat[index[1:k], ]
  dat <- getElement(dat, names(dat))
  dat <- dat[!is.na(dat)]

  if(random) return(sample(dat, 1))

  if (is.factor(dat) | is.character(dat)) return(mode_est(dat))

  mean(dat)

}

#' k-nearest-neighbor (knn) imputation
#' @export

knn_fill <- function(
  data,
  new_data,
  outcome,
  n_impute = 10,
  step_size = 1,
  neighbor_sequence = NULL,
  verbose = TRUE,
  ...
) {

  outcome <- vars_select(names(data), !!enquo(outcome))

  .dots <- list(...) %>%
    check_dots(valid_args = c('eps', 'nthread'))

  if( !('eps' %in% names(.dots)) ){
    .dots$eps <- 1e-08
  }

  if( !('nthread' %in% names(.dots)) ){
    .dots$nthread <- getOption("gd_num_thread")
  }


  if(nrow(data) == 0 || ncol(data)==0){
    stop("data is empty", call.=FALSE)
  }

  if(nrow(new_data) == 0 || ncol(new_data)==0){
    stop("new_data is empty", call.=FALSE)
  }

  neighbor_sequence <- neighbor_sequence %||%
    seq(
      from = step_size,
      to = n_impute * step_size,
      by = step_size
    )

  output <- knn_work(
    ref_data = data,
    new_data = new_data,
    outcome = outcome,
    neighbor_sequence = neighbor_sequence,
    nthread = .dots$nthread,
    epsilon = .dots$eps,
    verbose = verbose
  )

}

#' k-nearest-neighbor (knn) imputation
#' @description  Adapted from Max Kuhn's knn imputation functions in recipes
#' @export

knn_fit <- function(
  data,
  outcome,
  n_impute = 10,
  step_size = 1,
  neighbor_sequence = NULL,
  verbose = TRUE,
  ...
) {

  outcome <- vars_select(names(data), !!enquo(outcome))

  .dots <- list(...) %>%
    check_dots(valid_args = c('eps', 'nthread'))

  if( !('eps' %in% names(.dots)) ){
    .dots$eps <- 1e-08
  }

  if( !('nthread' %in% names(.dots)) ){
    .dots$nthread <- getOption("gd_num_thread")
  }


  if(nrow(data) == 0 || ncol(data)==0){
    stop("data is empty", call.=FALSE)
  }

  neighbor_sequence <- neighbor_sequence %||%
    seq(
      from = step_size,
      to = n_impute,
      by = step_size
    )

  output <- knn_work(
    ref_data = data,
    outcome = outcome,
    neighbor_sequence = neighbor_sequence,
    nthread = .dots$nthread,
    epsilon = .dots$eps,
    verbose = verbose
  )

}

knn_brew <- function(
  ref_data,
  new_data,
  nn_index,
  neighbors = 5
) {

  for(i in 1:ncol(new_data)) {

    na_index <- is.na(new_data[, i])

    if(any(na_index)){

      na_index <- which(na_index)

      new_data[na_index, i] <- apply(
        X = nn_index[, na_index],
        MARGIN = 2,
        FUN = nn_pred,
        dat = ref_data[, i, drop = FALSE],
        k = neighbors
      )

    }

  }

  new_data

}


knn_work <- function(
  ref_data,
  new_data = NULL,
  n_impute,
  neighbor_sequence,
  neighbor_aggr_fun,
  nthread = getOption("gd_num_thread"),
  epsilon = 1e-08,
  verbose = TRUE
) {

  new_data <- new_data %||% ref_data

  missing_rows <- !complete.cases(new_data)

  if ( !any(missing_rows) ) return(new_data)

  fits <- vector(mode = 'list', length = n_impute)

  for( i in seq(n_impute) ) fits[[i]] <- new_data

  for ( i in seq(ncol(new_data)) ) {

    imp_var <- names(new_data)[i]

    missing_rows <- !complete.cases(new_data[, imp_var])

    if ( any(missing_rows) ) {

      preds <- names(new_data)[-i]

      imp_data <- new_data[missing_rows, preds, drop = FALSE]

      ## do a better job of checking this:
      if (all(is.na(imp_data))) {

        warning(
          "All predictors are missing; cannot impute",
          call. = FALSE
        )

      } else {

        imp_var_complete <- !is.na(new_data[[imp_var]])

        n_complete <- sum(imp_var_complete)

        n <- min(
          n_complete,
          max(neighbor_sequence)
        )

        if(verbose){

          nthings <- min(length(preds), 4)

          print(
            glue(
              "imputing {imp_var} (nobs = {n_complete}) using \\
              {glue_collapse(preds[1:nthings], sep = ', ')}, \\
              and others"
            )
          )

        }

        nn_index <- gower_topn(
          x = imp_data[, preds],
          y = new_data[imp_var_complete, preds],
          n = n,
          nthread = nthread,
          eps = epsilon
        )$index

        pred_vals <- map(
          .x = neighbor_sequence,
          .f = ~ {

            if(verbose){
              print(glue("computing values using {.x} neighbors"))
            }

            apply(
              X = nn_index,
              MARGIN = 2,
              FUN = nn_pred,
              dat = new_data[imp_var_complete, imp_var],
              random = neighbor_aggr_fun == 'sample',
              k = .x
            )
          }
        )
        for(j in 1:n_impute){
          fits[[j]][missing_rows, imp_var] <- pred_vals[[j]]
        }
      }
    }
  }

  fits

}



# format_data_list(fits) %>%
#   mutate(neighbors = neighbor_sequence)
#
# format_data_list() %>%
#   mutate(
#     lambda = map_dbl(fits, ~attr(.x, 'lambda')),
#     rank = map_dbl(fits, ~attr(.x, 'rank'))
#   ) %>%
#   select(impute, lambda, rank, data)
