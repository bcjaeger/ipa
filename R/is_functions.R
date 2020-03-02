

#' Brew types
#'
#' Functions to help identify what stage a brew is in.
#'
#' @param x a brew object
#'
#' @return a logical value
#'
#' @export

is_brew <- function(x){
  inherits(x, 'ipa_brew')
}

#' @rdname is_brew
#' @export

is_spiced <- function(x){
  attr(x, 'spiced') %||% FALSE
}

#' @rdname is_brew
#' @export

is_mashed <- function(x){
  attr(x, 'mashed') %||% FALSE
}

#' @rdname is_brew
#' @export
is_fermented <- function(x){
  attr(x, 'fermented') %||% FALSE
}

#' @rdname is_brew
#' @export
is_bottled <- function(x){
  attr(x, 'bottled') %||% FALSE
}
