

# Turn this function into distill:

# 1. it should take a brew after
#  - sipping (variable accuracy) or
#  - chugging (model accuracy)

# 2. it will use the scored columns of the brew to rank imputes
#  - if the scores are variable specific, then different imputations
#    can be used to create a single imputed dataset (blending)
#  - if the scores are data-specific, then a single dataset from a
#    specific imputation is returned.

# 3. distill should return a list with
#  - the imputed training and testing data
#  - an object that you can plug into brew()

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
    dplyr::ungroup() %>%
    dplyr::select(variable, impute) %>%
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

ensemble_net <- function(z, y, alpha = 0.05, family = 'gaussian'){

  if(!is.matrix(z)) stop("z should be a matrix")

  if(!is.matrix(y)) stop("y should be a matrix")

  glmnet::glmnet(
    x = z,
    y = y,
    lambda = 0,
    alpha = alpha,
    lower.limits = 0,
    upper.limits = 1,
    intercept = FALSE,
    family = family
  ) %>%
    stats::coef() %>%
    as.matrix() %>%
    .[-1, , drop = FALSE] %>%
    magrittr::set_colnames('beta') %>%
    tibble::as_tibble(rownames = 'impute') %>%
    dplyr::mutate(beta = beta / sum(beta),
      impute = as.integer(impute)) %>%
    dplyr::filter(beta > 0)

}


blend_training <- function(brew, recipe = NULL) {

  .blend(brew = brew, with = 'training', recipe = recipe)

}

blend_testing <- function(brew, recipe = NULL) {

  .blend(brew = brew, with = 'testing', recipe = recipe)

}
