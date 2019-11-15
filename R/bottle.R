

#' Bottle a brew
#'
#' @param brew a `brew` object that has been passed through all
#'   the brewing steps: [brew], [spice], [mash], and [ferment].
#'
#' @param type a character value indicating what composition
#'   the training and testing data will be returned as.
#'
#' @param drop_fit (`TRUE` / `FALSE`). If `TRUE`, the column in
#'   the `wort` comprising imputation models will be dropped.
#'   Otherwise, the column will be retained.
#'
#' @export
bottle <- function(
  brew,
  type = c('tibble', 'matrix'),
  drop_fit = TRUE
) {

  check_brew(brew, expected_stage = 'bottle')

  .bottler <- switch (
    EXPR = type[1],
    'tibble' = .tibble_bottle,
    'matrix' = .matrix_bottle
  )

  bottles <- .bottler(brew)

  if(drop_fit) bottles[, 'fit'] = NULL

  bottles

}

.tibble_bottle <- function(brew){

  .cols <- attr(brew, 'fermented_cols')

  for( i in .cols ) brew$wort[[i]] %<>%
    purrr::map(dplyr::bind_cols, attr(brew, 'outcome')$data[[i]])

  brew$wort %<>% dplyr::mutate_at(
    .vars = .cols,
    .funs = ~purrr::map(.x, tibble::as_tibble)
  )

  brew$wort

}

.matrix_bottle <- function(brew){

  .cols <- attr(brew, 'fermented_cols')

  .list_mats <- function(...){
    list(...) %>%
      purrr::set_names(c('X','Y')) %>%
      purrr::map(as.matrix)
  }

  for( i in .cols ) brew$wort[[i]] %<>%
    purrr::map(.list_mats, attr(brew, 'outcome')$data[[i]])

  brew$wort

}

