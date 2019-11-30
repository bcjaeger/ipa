impute_train <- function(brew){

  UseMethod('impute_train')

}

impute_train.kneighbors_brew <- function(brew){
  brew$wort %<>% dplyr::mutate(training = fit)
  brew
}

impute_train.softImpute_brew <- function(brew){
  brew$wort %<>% dplyr::mutate(
    training = purrr::map(
      .x = fit,
      .f = ~ brew$data %>%
        softImpute::complete(.x, unscale = attr(brew, 'unscale')) %>%
        tibble::as_tibble()
    )
  )
  brew
}

impute_train.missRanger_brew <- function(brew){
  brew$wort %<>% dplyr::mutate(training = fit)
  brew
}
