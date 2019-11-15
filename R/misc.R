

list_things <- function(things){

  glue::glue_collapse(things, sep = ', ', last = ' and ')

}



