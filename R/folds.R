

#' @export
xgb_folds <- function(data, nfolds, strata){

  strata <- tidyselect::vars_select(names(data), !!enquo(strata))
  rsamp_folds <- vfold_cv(data, v=nfolds, strata = strata)
  map(rsamp_folds$splits, complement)

}

#' @export
stack_folds <- function(folds, n_impute){

  out <- folds %<>% map(
    .f = function(fold){
      fold %>%
        multiply_by(n_impute) %>%
        map(~seq(.x - n_impute + 1, .x)) %>%
        reduce(c)
    }
  ) %>%
    add_class_last("stacked_folds")

}

#' @export
list_folds <- function(folds, n_impute){

  out <- vector(mode = 'list', length = n_impute)
  for(i in seq_along(out)) out[[i]] <- folds
  out %<>% add_class_last("mi_folds")
  set_names(out, names(folds))

}
