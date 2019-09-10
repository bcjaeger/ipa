

list_things <- function(things){

  glue_collapse(things, sep = ', ', last = ' and ')

}


check_dots <- function(.dots, valid_args){

  bad_args <- setdiff(names(.dots), valid_args)

  if(!is_empty(bad_args)){
    stop(
      paste(
        "The following arguments are unrecognized:",
        list_things(bad_args)
      ),
      call. = FALSE
    )
  }

  .dots

}

#' from data list to tibble

format_data_list <- function(list){
  map(list, as_tibble) %>%
    set_names(1:length(list)) %>%
    enframe(name = 'impute', value = 'data') %>%
    mutate(impute = as.integer(impute))
}

#' add class to an object
add_class <- function(object, new_class){
  class(object) %<>% c(new_class)
  object
}


