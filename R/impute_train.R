impute_train <- function(brew){

  UseMethod('impute_train')

}

impute_train.kneighbors_brew <- function(brew){

  brew$wort %<>% dplyr::rename(training = fit)

  brew

}

impute_train.softImpute_brew <- function(brew){

  brew$wort %<>% dplyr::mutate(
    training = purrr::map(
      .x = fit,
      .f = ~ brew$data %>%
        softImpute::complete(
          x = .x,
          unscale = attr(brew, 'ferment_args')$unscale
        ) %>%
        tibble::as_tibble()
    )
  )

  brew$fit <- NULL

  brew

}

impute_train.missRanger_brew <- function(brew){

  brew$wort %<>% dplyr::rename(training = fit)

  brew

}
