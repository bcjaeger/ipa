#' @export
stack_params <- function(pars, n_impute){

  if( !('subsample' %in% names(pars)) ){
    pars$subsample <- 1 / n_impute
  } else {
    pars$subsample %<>% divide_by(n_impute)
  }

  class(pars) %<>% c("stacked_params")

  pars

}

#' @export
list_params <- function(pars, n_impute){

  new_pars <- vector(mode='list', length = n_impute)
  for(i in seq_along(new_pars)) new_pars[[i]] <- pars

  class(new_pars) %<>% c("mi_params")

  new_pars

}
