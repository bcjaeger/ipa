drop_empty <- function(x){

  stopifnot(is.list(x))

  cells_to_drop <- which(purrr::map_lgl(x, is_empty))

  if(!is_empty(cells_to_drop)) x[-cells_to_drop] else x

}

is_empty <- function (x) length(x) == 0

list_things <- function(things){

  glue::glue_collapse(things, sep = ', ', last = ' and ')

}

text_pillar <- function(lhs, rhs, middle){

  fun_indx <- which(purrr::map_lgl(rhs, is.function))

  if(!is_empty(fun_indx)){
    rhs <- rhs[-fun_indx]
    lhs <- lhs[-fun_indx]
  }

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

df_unique_indx <- function(fit_dfs){
  runs <- rle(fit_dfs)
  cumsum(runs$lengths)
}
