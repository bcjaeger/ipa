
mindx <- function(data,
  sep = '_',
  miss_chr = 'missing',
  drop_const = FALSE,
  attach = FALSE
) {

  miss_cols <- data %>%
    purrr::map_dfc(.f = ~as.integer(is.na(.x))) %>%
    purrr::set_names(glue::glue("{names(.)}{sep}{miss_chr}"))

  if(drop_const){
    keep <- names(which(purrr::map_lgl(miss_cols, ~length(unique(.x)) > 1)))
  } else {
    keep <- names(miss_cols)
  }

  if(attach) cbind(data, miss_cols[, keep]) else miss_cols[, keep]

}

