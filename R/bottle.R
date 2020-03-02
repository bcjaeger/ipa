

#' Bottle a brew
#'
#' @param brew a `brew` object that has been passed through all
#'   the brewing steps: [brew], [spice], [mash], and [ferment].
#'
#' @param type a character value indicating what composition
#'   the training and testing data should have. Valid options
#'   are 'tibble' and 'matrix'
#'
#' @param drop_fit (`TRUE` / `FALSE`). If `TRUE`, the column in
#'   the `wort` comprising imputation models will be dropped.
#'   Otherwise, the column will be retained.
#'
#' @export
bottle <- function(
  brew,
  type = 'tibble',
  drop_fit = TRUE
) {

  check_brew(brew, expected_stage = 'bottle')
  check_chr(type, label = 'type', options = c('tibble', 'matrix'))
  check_l1_stop(type, label = 'type')
  check_bool(drop_fit, label = 'drop_fit')
  check_l1_stop(drop_fit, label = 'drop_fit')

  brew <- switch (
    EXPR = type,
    'tibble' = .tibble_bottle(brew),
    'matrix' = .matrix_bottle(brew)
  )

  if(drop_fit & 'fit' %in% names(brew$wort)){
    brew$wort$fit <- NULL
  }

  if(get_flavor(brew) == 'softImpute') {

    brew$wort <- dplyr::mutate(
      brew$wort,
      pars = apply(cbind(lambda, rank_max, rank_fit), 1, as.list)
    ) %>%
      dplyr::select(-c(lambda, rank_max, rank_fit))

  }

  if(get_flavor(brew) == 'kneighbors') {

    brew$wort <- dplyr::mutate(
      brew$wort,
      pars = apply(cbind(k_neighbors), 1, as.list),
    ) %>%
      dplyr::select(-c(k_neighbors))

  }

  brew$wort <-  brew$wort %>%
    dplyr::select(impute, pars, dplyr::everything())

  brew

}


.tibble_bottle <- function(brew){


  .cols <- attr(brew, 'fermented_cols')

  for( i in .cols ) brew$wort[[i]] <- brew$wort[[i]] %>%
    purrr::map(
      .f = ~ dplyr::bind_cols(attr(brew, 'outcome')$data[[i]], .x)
    )

  brew$wort <- tibble::as_tibble(brew$wort) %>%
    dplyr::mutate_at(
      .vars = .cols,
      .funs = ~purrr::map(.x, tibble::as_tibble)
    )

  if(get_bind_miss(brew)){

    for(f in attr(brew, 'fermented_cols')){

      brew$wort[[f]] <- purrr::map(
        .x = brew$wort[[f]],
        .f = ~ dplyr::bind_cols(.x, brew$miss[[f]])
      )

    }

  }

  attr(brew, 'bottled') <- TRUE
  attr(brew, 'composition') <- 'tibble'

  brew

}

.matrix_bottle <- function(brew){

  .cols <- attr(brew, 'fermented_cols')

  .list_mats <- function(...){
    list(...) %>%
      purrr::set_names(c('X','Y')) %>%
      purrr::map(as.matrix)
  }

  brew$wort <- tibble::as_tibble(brew$wort)

  for( i in .cols ) brew$wort[[i]] <- brew$wort[[i]] %>%
    purrr::map(.list_mats, attr(brew, 'outcome')$data[[i]])

  if(get_bind_miss(brew)){

    for(f in attr(brew, 'fermented_cols')){

      brew$wort[[f]] <- purrr::map(
        .x = brew$wort[[f]],
        .f = ~ {
          .x$X <- cbind(.x$X, as.matrix(brew$miss[[f]]))
          .x
        }
      )

    }

  }

  attr(brew, 'bottled') <- TRUE
  attr(brew, 'composition') <- 'matrix'

  brew

}


