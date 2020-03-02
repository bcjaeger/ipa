

list_things <- function(things){

  glue::glue_collapse(things, sep = ', ', last = ' and ')

}

text_pillar <- function(lhs, rhs, middle){

  rhs <- purrr::map_chr(rhs, .f = function(x){
    if(is.logical(x)) x <- as.character(x)
    if(is.numeric(x)) x <- as.character(round(x, digits = 2))

    to_list <- min(length(x), 3)

    paste0(glue::glue_collapse(x[seq(to_list)], sep = ', '),
      if(to_list > 1) '...'
    )

  })

  paste0('<', lhs, '> ', middle, ' <',
    rhs, '>', collapse = '\n')

}



