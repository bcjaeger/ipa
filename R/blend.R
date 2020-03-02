

blend_recipe <- function(brew, with = 'training'){

  .with <- names(brew$wort) %>%
    tidyselect::vars_select(!!rlang::enquo(with)) %>%
    purrr::set_names(NULL) %>%
    paste0('_score')

  brew$wort %>%
    dplyr::select_at(tidyselect::all_of(c('impute', .with))) %>%
    dplyr::rename(.with = .with) %>%
    tidyr::unnest(.with) %>%
    dplyr::group_by(variable) %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice(1L) %>%
    dplyr::arrange(variable)

}

.blend <- function(brew, with, recipe=NULL){

  recipe <- recipe %||% blend_recipe(brew, with)

  imputes <- purrr::map2_dfc(
    purrr::set_names(as.character(recipe$variable)),
    recipe$impute,
    .f = ~ {
      brew$wort %>%
        dplyr::filter(impute == .y) %>%
        dplyr::select_at(tidyselect::all_of(c('impute', with))) %>%
        dplyr::rename(with = with) %>%
        tidyr::unnest(with) %>%
        purrr::pluck(.x)
    }
  )

  outcome <- get_outcome(brew)$data[[with]]

  dplyr::bind_cols(outcome, imputes)

}

blend_training <- function(brew, recipe = NULL) {

  .blend(brew = brew, with = 'training', recipe = recipe)

}

blend_testing <- function(brew, recipe = NULL) {

  .blend(brew = brew, with = 'testing', recipe = recipe)

}
