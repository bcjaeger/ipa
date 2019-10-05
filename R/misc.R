
#' easy lists of things
list_things <- function(things){

  glue_collapse(things, sep = ', ', last = ' and ')

}

#' from data list to tibble
format_data_list <- function(list){

  map(list, as_tibble) %>%
    set_names(1:length(list)) %>%
    enframe(name = 'impute', value = 'data') %>%
    mutate(impute = as.integer(impute))

}

#' from tibble to data list
prep_data_list <- function(data, n_impute){

  if( is.null(n_impute) ){
    stop("n_impute needs to be specified", call. = FALSE)
  }

  n_obs <- nrow(data) / n_impute
  data_list <- vector(mode='list', length = n_impute)

  for(i in seq_along(data_list)){

    start <- 1 + n_obs * (i-1)
    stop <- n_obs * i
    data_list[[i]] <- data[start:stop, ]

  }

  data_list

}


#' add attributes to an object
add_attrs <- function(object, ...){

  .dots <- list(...)

  for(i in seq_along(.dots)){
    attr(object, names(.dots)[i]) <- .dots[[i]]
  }

  object


}

#' add class to an object
add_class_first <- function(object, new_class){
  class(object) %<>% c(new_class)
  object
}

#' add class to an object
add_class_last <- function(object, new_class){
  class(object) %<>% c(., new_class)
  object
}


#' transfer factor levels
#'
#' @description take the factor levels in training data
#'   and copy them over to testing data. This is an important
#'   pre-processing step for data splits that may have
#'   different factor levels in training and testing sets.
#'
#' @param to the data that factor levels are transferred to
#' @param from the data that factor levels are transferred from
#'
#' @note `to` and `from` must have the same factor columns. For example,
#'   if `to` has a factor named `A` and `from` does not have a factor
#'   of the same name, the function will stop and tell you which
#'   factor variables are missing.
#'
#' @export
#'
transfer_factor_levels <- function(to, from){

  # check that the two frames have the same factor variables

  fctrs_to <- get_factors(to)
  fctrs_from <- get_factors(from)

  fctrs_only_in_to <- setdiff(fctrs_to, fctrs_from)
  fctrs_only_in_from <- setdiff(fctrs_from, fctrs_to)

  if(!is_empty(fctrs_only_in_to)){
    stop(
      paste(
        "to some factors that are not in from:",
        list_things(fctrs_only_in_to)
      )
    )
  }

  if(!is_empty(fctrs_only_in_from)){
    stop(
      paste(
        "from has some factors are not in to:",
        list_things(fctrs_only_in_from)
      )
    )
  }

  levels_from <- map(
    .x = set_names(fctrs_from, fctrs_from),
    .f = ~ levels(from[[.x]])
  )

  for(f in names(levels_from)){
    to[[f]] %<>% factor(levels = levels_from[[f]])
  }

  return(to)

}

