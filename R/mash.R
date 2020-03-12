

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
#' @param with a helper function for mashing brews.
#'   See [masher_nbrs] and [masher_soft])
#'
#' @return an `ipa_brew` object with additional values attached to `pars`.
#'
#' @export
#'
#' @examples
#'
#' x1 = rnorm(100)
#' x2 = rnorm(100) + x1
#' x3 = rnorm(100) + x1 + x2
#'
#' outcome = 0.5 * (x1 - x2 + x3)
#'
#' data <- data.frame(x1=x1, x2=x2, x3=x3, outcome=outcome)
#'
#' n_miss = 10
#'
#' data[1:n_miss,'x1'] = NA
#' sft_brew <- brew_soft(data, outcome=outcome, bind_miss = FALSE)
#'
#' # these two calls are equivalent
#' mash(sft_brew, with = masher_soft(bs = FALSE))
#' mash(sft_brew, bs = FALSE)
#'
#' knn_brew <- brew_nbrs(data, outcome=outcome, bind_miss = TRUE) %>%
#'
#' # these two calls are equivalent
#' mash(knn_brew, with = masher_nbrs(fun_aggr_ctns = median))
#' mash(knn_brew, fun_aggr_ctns = median)
#'

mash <- function(brew, with = NULL, ...){

  UseMethod('mash')

}

#' @describeIn mash Mash a soft brew (use [masher_soft] for `with`).
#' @export
#'
mash.softImpute_brew <- function(brew, with = NULL, ...){

  # if someone wants to go straight from brew to mash
  # then the brew will use the default spices.

  if(!is_spiced(brew)){

    check_brew(brew, expected_stage = 'spice')
    brew <- spice(brew, with = spicer_soft())

    if(get_verbosity(brew) > 0) message("Default spices are being used:\n",
      text_pillar(lhs = names(brew$pars), rhs = brew$pars,
        middle = 'has value(s)'))

  }

  check_masher(with, expected = 'masher_soft')

  .dots <- with %||% check_dots(
      list(...),
      valid_args = c(
        'bs',
        'bs_maxit',
        'bs_thresh',
        'bs_row.center',
        'bs_col.center',
        'bs_row.scale',
        'bs_col.scale',
        'si_type',
        'si_thresh',
        'si_maxit',
        'si_final.svd'
      )
    )

  # set default values if nothing was specified
  .dots$bs            <- .dots$bs            %||% TRUE
  .dots$bs_maxit      <- .dots$bs_maxit      %||% 20
  .dots$bs_thresh     <- .dots$bs_thresh     %||% 1e-09
  .dots$bs_row.center <- .dots$bs_row.center %||% FALSE
  .dots$bs_col.center <- .dots$bs_col.center %||% TRUE
  .dots$bs_row.scale  <- .dots$bs_row.scale  %||% FALSE
  .dots$bs_col.scale  <- .dots$bs_col.scale  %||% TRUE
  .dots$si_type       <- .dots$si_type       %||% 'als'
  .dots$si_thresh     <- .dots$si_thresh     %||% 1e-05
  .dots$si_maxit      <- .dots$si_maxit      %||% 100
  .dots$si_final.svd  <- .dots$si_final.svd  %||% TRUE

  # softImpute and biScale have trace parameters that show
  # printed output for each iteration of the corresponding function.
  # if verbose level is 2, we'll show that output. If verbose level
  # is 1, we'll just show messages from ipa functions.
  .dots$si_trace <- .dots$bs_trace <- get_verbosity(brew) > 1

  attr(brew, 'mashed')  <- TRUE
  brew$pars <- c(brew$pars, .dots)
  brew

}

#' @describeIn mash Mash a neighbor's brew (use [masher_nbrs] for `with`)
#' @export
#'
mash.kneighbors_brew <- function(brew, with = NULL, ...){

  if(!is_spiced(brew)){

    check_brew(brew, expected_stage = 'spice')
    brew <- spice(brew, with = spicer_nbrs())

    if(get_verbosity(brew) > 0) message("Default spices are being used:\n",
      text_pillar(lhs = names(brew$pars), rhs = brew$pars,
        middle = 'has value(s)'))

  }

  check_masher(with, expected = 'masher_nbrs')

  .dots <- with %||% check_dots(
    list(...),
    valid_args = c(
      'epsilon',
      'nthread',
      'fun_aggr_ctns',
      'fun_aggr_intg',
      'fun_aggr_catg'
    )
  )

  # set default values if nothing was specified
  .dots$epsilon       <- .dots$epsilon       %||% 1e-08
  .dots$nthread       <- .dots$nthread       %||% getDTthreads()
  .dots$fun_aggr_ctns <- .dots$fun_aggr_ctns %||% NULL
  .dots$fun_aggr_intg <- .dots$fun_aggr_intg %||% NULL
  .dots$fun_aggr_catg <- .dots$fun_aggr_catg %||% NULL

  # softImpute and biScale have trace parameters that show
  # printed output for each iteration of the corresponding function.
  # if verbose level is 2, we'll show that output. If verbose level
  # is 1, we'll just show messages from ipa functions.
  .dots$verbose <- get_verbosity(brew) > 0

  attr(brew, 'mashed')  <- TRUE
  brew$pars <- c(brew$pars, .dots)
  brew

}


#' Soft masher
#' @inherit masher_nbrs description return
#' @inherit mash examples
#' @inheritParams impute_soft
#' @export

masher_soft <- function(
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
  structure(
    .Data = list(bs = bs,
      bs_maxit = bs_maxit,
      bs_thresh = bs_thresh,
      bs_row.center = bs_row.center,
      bs_col.center = bs_col.center,
      bs_row.scale = bs_row.scale,
      bs_col.scale = bs_col.scale,
      si_type = si_type,
      si_thresh = si_thresh,
      si_maxit = si_maxit,
      si_final.svd = si_final.svd),
    class = 'masher_soft'
  )
}

#' Neighbor's masher
#'
#'
#' @description
#' If you use Rstudio, the `masher` and `spicer` functions can help
#'  remind you which parameters go along with which `ipa_brew` flavor.
#'  The basic idea is to write `spice(brew, with = spicer_<flavor>())`
#'  and `mash(brew, with = masher_<flavor>())`. Hitting the tab key with
#'  your curser inside the parentheses of `masher_flavor()`will create a
#'  drop-down menu that shows a list of the arguments that go along with
#'  your brew's flavor.
#'
#' If you have no trouble remembering the parameters that go along
#'  with your brew's flavor, or if you just want your code to be more concise,
#'  you don't have to use the `with` argument. Instead, you can just
#'  specify parameter values directly using the `...` argument in the `mash`
#'  and `spice` functions. In the examples below, both approaches are shown.
#'
#' @inheritParams impute_nbrs
#'
#' @return a list with input values that can be passed directly into
#'   [mash], e.g `mash(brew, with = masher_nbrs())` for a neighbors brew or
#'   `mash(brew, with = masher_soft())` for a soft brew.
#'
#' @inherit mash examples
#'
#' @export
#'

masher_nbrs <- function(
  epsilon = 1e-08,
  nthread = getOption("gd_num_thread"),
  fun_aggr_ctns = mean,
  fun_aggr_intg = medn_est,
  fun_aggr_catg = mode_est
){

  structure(
    .Data = list(epsilon = epsilon,
      nthread = nthread,
      fun_aggr_ctns = fun_aggr_ctns,
      fun_aggr_intg = fun_aggr_intg,
      fun_aggr_catg = fun_aggr_catg
    ),
    class = 'masher_nbrs'
  )

}


is_masher <- function(x){
  inherits(x, paste("masher", c('nbrs','rngr','soft'), sep = '_'))
}

