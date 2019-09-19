
#' get factor variables from data
#'
#' @param data data with factor variables
#' @export

get_factors <- function(data){
  sapply(data, is.factor) %>%
    which() %>%
    names()
}


#' get factor levels from data
#'
#' @param data data with factor variables
#' @export


get_factor_levels <- function(data){

  factor_variables <- get_factors(data)

  factor_variables %>%
    set_names(factor_variables) %>%
    map(~levels(data[[.x]]))
}

#' set/get missing strategy
#'
#' @description Missing strategies may include single imputation (`si`),
#'   multiple imputation (`mi`), or multiple imputation for decision
#'   trees (`midy`). It is helpful to set missing strategy = `si`
#'   when using `midy` functions to work with the default `xgboost`
#'   strategy to handle missing values. At the very least, it avoids
#'   confusion and prevents warning messages.
#'
#' @note it is highly inadvisable to change the missing strategy
#'   of a `midy` object or an `mi` object. This will change the
#'   class of the object, which is an essential input to other
#'   functions in the `midy` package such as \code{\link{mgb_cv}}
#'   and \code{\link{mgb_predict}}.
#'
#' @param object an object of class data.frame, matrix, or xgb.DMatrix
#' @param miss_strat a character value indicating the missing strategy
#'   that will be set for `object`. Valid inputs are 'si', 'mi', and
#'   'midy'.
#'
#' @export
set_miss_strat <- function(object, miss_strat){

  type <- c('data.frame','matrix','xgb.DMatrix') %>%
    purrr::set_names() %>%
    map_lgl(~inherits(object, .x))

  if(sum(type) != 1){
    stop(
      "unable to set a missing strategy for this object",
      call. = FALSE
    )
  }

  type = names(type)[which(type)]

  type <- switch(
    type,
    'data.frame' = 'data',
    'matrix' = 'xmat',
    'xgb.DMatrix' = 'dmat'
  )

  if(miss_strat == 'si') attr(object, 'nimpute') <- 1

  new_class <- paste0(miss_strat, '_', type)

  class(object) <- c(class(object), new_class)

  object

}

#' @rdname set_miss_strat
#' @export
get_miss_strat <- function(object){

  types <- c('data','xmat','dmat')

  cls <- c(
    si = inherits(object, paste0('si_', types)),
    list = inherits(object, paste0('list_', types)),
    stack = inherits(object, paste0('stack_', types))
  )

  if (sum(cls) > 1) {
    stop(
      "multiple missing strategies specified. Only one should be listed",
      call. = FALSE
    )
  }

  if (sum(cls) == 0) {
    # warning(
    #   "Missing strategy class not found, single imputation is assumed",
    #   call. = FALSE
    # )
    return("si")

  }

  names(cls)[which(cls)]


}



get_status <- function(mgb_surv_label){

  out <- rep(0, length(mgb_surv_label))
  out[mgb_surv_label > 0] = 1
  out

}

get_time <- function(mgb_surv_label){

  abs(mgb_surv_label)

}



