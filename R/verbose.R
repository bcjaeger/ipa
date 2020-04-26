#' Noisier brewing
#'
#' Sometimes you want to see text output to make sure
#'   your brew is brewing the way you want it to. Use this function
#'   to make your brew tell you what's going on at all subsequent
#'   stages.
#'
#' @param brew an `ipa_brew` object (see [brew]).
#' @param level The level of verbosity.
#'
#' @return a noisier `ipa_brew` object with
#'   an adjusted `verbose` attribute value.
#'
#' @export
#'

verbose_on <- function(brew, level){

  if(!is_brew(brew)) stop("the input is not an ipa_brew object")

  if(!level %in% c(1:2)) stop(
    'level should be 1 (a little verbose) or 2 (a lot verbose).',
    '\nIf you want verbosity to be 0, please use verbose_off()'
  )

  message(switch(level,
    '1' = "your brew will now tell you generally what it's doing.",
    '2' = "your brew will now tell you everything that it's doing."
  ))

  attr(brew, 'verbose') <- level

  brew

}

#' @rdname verbose_on
#' @export
verbose_off <- function(brew){

  message("your brew will no longer print any messages.")

  attr(brew, 'verbose') <- 0

  brew

}


#' @rdname verbose_on
#' @export
get_verbosity <- function(brew) attr(brew, 'verbose')
