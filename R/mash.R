

#' Mash a brew
#'
#' @description When an `ipa_brew` is mashed, training data are
#'   used to fit imputation models based on the brew flavor.
#'
#' @param brew an `ipa_brew` object that may or may not have
#'   been spiced (see [spice]).
#'
#' @param ... additional arguments for specific brew flavors.
#'
#' @param with the output of a helper function for mashing brews.
#'   The helper functions are [masher_nbrs], [masher_soft], and
#'   [masher_rngr].
#'
#' @export
#'
#' @examples
#'
#' set.seed(101)
#' n=200
#' p=100
#' J=50
#' np=n*p
#' missfrac=0.3
#' x=matrix(rnorm(n*J),n,J)%*%matrix(rnorm(J*p),J,p)+matrix(rnorm(np),n,p)/5
#' ix=seq(np)
#' imiss=sample(ix,np*missfrac,replace=FALSE)
#' xna=x
#' xna[imiss]=NA
#'
#' data <- as.data.frame(xna)
#'
#' soft_brew <- brew(data, outcome = V1, flavor = 'softImpute')
#' soft_brew <- spice(soft_brew, n_impute = 10, step_size = 2)
#' soft_brew <- mash(soft_brew, scale_lambda = 0.12)
#'
#' soft_brew$wort

mash <- function(brew, with = NULL, ...){

  UseMethod('mash')

}

check_mash <- function(brew, verbose){

  if(!is_spiced(brew)){
    brew <- simple_spice(brew)
    if(verbose > 0) message(
      "Looks like this brew hasn't been spiced yet.\n",
      "I will spice it for you using default values.\n",
      "Take a look at <your brew>$pars to see these values.\n"
    )
  }

}

#' @describeIn mash Mash a soft brew (use [masher_soft] for `with`).
#' @export
#'
mash.softImpute_brew <- function(brew, with = NULL, ...){


  verbose <- get_verbosity(brew)
  verbose_1 <- verbose >= 1
  verbose_2 <- verbose >= 2

  check_mash(brew, verbose_1)

  .dots <- with %||% check_dots(
      list(...),
      valid_args = c(
        'type',
        'maxit',
        'thresh',
        'final.svd',
        'scale_data',
        'scale_iter',
        'scale_lambda',
        'lambda_sequence'
      )
    )

  .dots$type <- .dots$type %||% "als"
  .dots$maxit <- .dots$maxit %||% 100L
  .dots$thresh <- .dots$thresh %||% 1e-05
  .dots$trace.it <- verbose_2
  .dots$final.svd <- .dots$final.svd %||% TRUE
  .dots$scale_data <- .dots$scale_data %||% TRUE
  .dots$scale_iter <- .dots$scale_iter %||% 20
  .dots$scale_lambda <- .dots$scale_lambda %||% 0.95

  check_pos_int(
    .dots$scale_iter,
    label = 'number of iterations for bi-scale'
  )

  check_fraction(
    .dots$scale_lambda,
    'scale_lambda'
  )

  if(!is.logical(.dots$scale_data))
    stop('scale_data should be a single logical value ',
      'e.g. TRUE or FALSE', call. = FALSE)

  if(verbose_1)
    message(
      glue::glue(
        "Identifying max lambda and scaling by {.dots$scale_lambda}"
      )
    )

  lam0 <- softImpute::lambda0(
    x = brew$data,
    trace.it = verbose_2
  ) %>%
    magrittr::multiply_by(.dots$scale_lambda)

  lamseq <- .dots$lambda_sequence %||%
    seq(lam0, 1, length.out = brew$pars$n_impute)

  fits = as.list(lamseq)
  ranks = as.integer(lamseq)
  rank.max = rank_max = brew$pars$max_rank
  warm = NULL

  args <- list(
    x = as.matrix(brew$data),
    type = .dots$type,
    thresh = .dots$thresh,
    maxit = .dots$maxit,
    trace.it = .dots$trace.it,
    final.svd = .dots$final.svd
  )

  if(.dots$scale_data){

    if(verbose_1){
      message("Applying biScale() to data")
    }

    args$x <- try(
      softImpute::biScale(
        x = args$x,
        maxit = .dots$scale_iter,
        trace = verbose_1
      ),
      silent = TRUE
    )

    if(class(args$x)[1] == 'try-error') stop(
      "unable to run biScale on brew data",
      call. = FALSE
    )

  }

  if(verbose_1) message(
    "Fitting soft-impute models"
  )

  for( i in seq_along(lamseq) ){

    args$lambda <- lamseq[i]
    args$rank.max = rank.max
    args$warm.start = warm

    fiti <- do.call(softImpute::softImpute, args = args)

    attr(fiti, 'rank') <- ranks[i] <- sum(round(fiti$d, 4) > 0)
    rank.max = min(ranks[i] + brew$pars$step_size, rank_max)
    warm = fiti
    fits[[i]] = fiti

    if(verbose_1){

      print(
        glue::glue(
          "fit {i} of {brew$pars$n_impute}: \\
            lambda = {format(round(lamseq[i], 3),nsmall=3)}, \\
            rank.max = {rank.max} \\
            rank.fit = {ranks[i]}"
        )
      )

    }

  }

  brew$wort <- tibble::enframe(fits, name = 'impute', value = 'fit') %>%
    dplyr::mutate(lambda = lamseq, rank = ranks) %>%
    dplyr::select(impute, lambda, rank, fit)

  attr(brew, 'mashed') = TRUE
  attr(brew, 'unscale') <- .dots$scale_data

  brew

}

#' @describeIn mash Mash a neighbor's brew (use [masher_nbrs] for `with`)
#' @export
#'
mash.kneighbors_brew <- function(brew, with = NULL, ...){

  verbose <- get_verbosity(brew)
  verbose_1 <- verbose >= 1
  verbose_2 <- verbose >= 2

  check_mash(brew, verbose_1)

  .dots <- with %||% check_dots(
    list(...),
    valid_args = c('eps', 'nthread')
  )

  .dots$eps <- .dots$eps %||% 1e-08
  .dots$nthread <- .dots$nthread %||% getOption("gd_num_thread")
  .dots$verbose <- verbose_1
  .dots$ref_data <- brew$data
  .dots$n_impute <- length(brew$pars$nbrs)
  .dots$neighbor_sequence <- brew$pars$nbrs
  .dots$neighbor_aggregate <- brew$pars$aggr

  fits <- do.call(knn_work, args = .dots)

  brew$wort <- fits %>%
    tibble::enframe(name = 'impute', value = 'fit') %>%
    dplyr::mutate(
      k_neighbors = brew$pars$nbrs,
      aggr_fun = dplyr::if_else(brew$pars$aggr,
        true = 'mean_mode',
        false = 'donor'
      )
    ) %>%
    dplyr::select(impute, k_neighbors, aggr_fun, fit)

  attr(brew, 'mashed') = TRUE

  brew

}

#' @describeIn mash Mash a ranger's brew (use [masher_rngr] for `with`)
#' @export
#'
mash.missRanger_brew <- function(brew, with = NULL, ...){

  verbose <- get_verbosity(brew)

  check_mash(brew, verbose>0)

  .dots <- with %||% check_dots(
    list(...),
    valid_args = c(
      'maxiter',
      'num.trees',
      'sample.fraction'
    )
  )

  .dots$verbose <- verbose
  .dots$data <- brew$data

  fits <- purrr::map2(
    .x = brew$pars$node_size,
    .y = brew$pars$donor_size,
    .f = ~ {
      .dots$pmm.k <- .y
      .dots$min.node.size <- .x
      do.call(missRanger::missRanger, args = .dots)
    }
  )

  brew$wort <- fits %>%
    tibble::enframe(name = 'impute', value = 'fit') %>%
    dplyr::mutate(
      node_size = brew$pars$node_size,
      donor_size = brew$pars$donor_size
    ) %>%
    dplyr::select(impute, node_size, donor_size, fit)

  attr(brew, 'mashed') = TRUE

  brew

}


#' Soft masher
#'
#' It can be a little overwhelming to remember which sets of
#' parameters go with each `ipa_brew` flavor, so just pair your
#' flavor with its `masher` function and get on with your `brew`.
#'
#' @param type two algorithms are implements, type="svd" or the default
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
#' @param maxit maximum number of iterations for the `softImpute`
#'   algorithm.
#'
#' @param thresh convergence threshold, measured as the relative
#'  change in the Frobenius norm between two successive estimates.
#'
#' @param final.svd only applicable to type="als". The alternating
#'  ridge-regressions do not lead to exact zeros. With the default
#'  final.svd=TRUE, at the final iteration, a one step unregularized
#'  iteration is performed, followed by soft-thresholding of the
#'  singular values, leading to hard zeros.
#'
#' @param scale_data (`TRUE` / `FALSE`) if `TRUE`, the
#'   [biScale][softImpute::biScale()] function will be applied
#'   to `data` before soft imputation is applied.
#'
#' @param scale_iter the maximum number of iterations for the
#'   `biScale` algorithm.
#'
#' @param scale_lambda A number that will scale the maximum value of
#'   `lambda`, a regularization parameter for `softImpute`.
#'
#' @param lambda_sequence A numeric vector of `lambda` values.
#'   This input is optional. If it is not specified, a sequence
#'   of `lambda` values will be generated automatically.
#'
#' @return a list with [softImpute][softImpute::softImpute()] and
#'   [biScale][softImpute::biScale()] inputs values that can be
#'   passed directly into `softImpute_brew` objects via [mash].
#'
#' @export
#'

masher_soft <- function(
  type = 'als',
  maxit = 100L,
  thresh = 1e-05,
  final.svd = TRUE,
  scale_data = TRUE,
  scale_iter = 20,
  scale_lambda = 0.95,
  lambda_sequence = NULL
){
  structure(
    .Data = list(
      type = type,
      maxit = maxit,
      thresh = thresh,
      final.svd = final.svd,
      scale_data = scale_data,
      scale_iter = scale_iter,
      scale_lambda = scale_lambda,
      lambda_sequence = lambda_sequence
      ),
    class = 'masher_soft'
  )
}

#' Ranger's masher
#'
#' It can be a little overwhelming to remember which sets of
#' parameters go with each `ipa_brew` flavor, so just pair your
#' flavor with its `masher` function and get on with your `brew`.
#'
#'
#' @param maxiter An integer specifying the maximum number of
#'  iterations in the [missRanger][missRanger::missRanger()] algorithm.
#' @param num.trees An integer specifying the number of decision
#'  trees fitted for each random forest. Lower values of `num.trees`
#'  will increase computation speed but may decrease accuracy.
#' @param sample.fraction A fraction indicating the proportion of
#'  data randomly sampled for each decision tree. Lower values of
#'  `sample.fraction` will increase computation speed but may
#'  decrease accuracy.
#'
#' @return a list with [missRanger][missRanger::missRanger()] and
#'   [ranger][ranger::ranger()] input values that can be passed
#'   directly into `missRanger_brew` objects via [mash].
#'
#' @export
#'


masher_rngr <- function(
  maxiter = 10L,
  num.trees = 100,
  sample.fraction = 0.632
){
  structure(
    .Data = list(maxiter = maxiter,
      num.trees = num.trees,
      sample.fraction = sample.fraction),
    class = 'masher_rngr'
  )
}

#' Neighbor's masher
#'
#' It can be a little overwhelming to remember which sets of
#' parameters go with each `ipa_brew` flavor, so just pair your
#' flavor with its `masher` function and get on with your `brew`.
#'
#' @param eps A numeric value > 0. Computed numbers (variable ranges)
#'   smaller than `eps` are treated as zero
#'
#' @param nthread Number of threads to use for parallelization.
#'  By default, for a dual-core machine, 2 threads are used.
#'  For any other machine n-1 cores are used so your machine doesn't
#'  freeze during a big computation. The maximum number of threads
#'  are determined using `omp_get_max_threads` at C level.
#'
#' @return a list with [gower_topn][gower::gower_topn()] input values
#'  that can be passed directly into `kneighbors_brew` objects via [mash].
#'
#' @export
#'

masher_nbrs <- function(eps=1e-08, nthread = getOption("gd_num_thread")){

  structure(
    .Data = list(eps = eps, nthread = nthread),
    class = 'masher_nbrs'
  )

}


#' @rdname mash
#' @export

is_mashed <- function(brew){
  attr(brew, 'mashed')
}

