
get_factor_info <- function(data){

  is_cat <- function(x) is.factor(x) & !is.ordered(x)

  fctr_info <- vector(mode = 'list', length = 3L)
  names(fctr_info) <- c('cols', 'lvls', 'keys')

  fctr_info$cols <- names(data)[which(sapply(data, is_cat))]

  fctr_info$lvls <- fctr_info$cols %>%
    purrr::set_names() %>%
    purrr::map(~levels(data[[.x]]))

  fctr_info$keys <- purrr::map2(
    purrr::set_names(fctr_info$cols),
    fctr_info$lvls,
    paste, sep = '_'
  )

  fctr_info

}
