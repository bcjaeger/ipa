

#' Prepare data stacks/lists
#'
#' @description stacked dataframes comprise multiply imputed
#'   dataframes that are stacked on top of one another. Imputed values in
#'   these objects are usually a model prediction + random noise.
#'   The additional noise creates diversity among the multiple
#'   dataframes. Data lists are similar, but data frames are kept
#'   separate and one model is fit for each dataframe.
#'
#'   These functions create ID variables and attach classes and attributes
#'   that are used to direct downstream analyses.
#'
#' @param data (a list) This argument should be a list of
#'   imputed dataframes, each having the same number of rows and columns.
#'
#' @export
#'
#' @examples
#' data_list <- list(
#'   df1 = data.frame(a = 1:3, b = letters[1:3]),
#'   df2 = data.frame(a = 2:4, b = letters[2:4])
#' )
#'
#' stack_data(data_list)
#'
#' as_mi(data_list)
#'
#' as_si(data_list[[1]])

stack_data <- function(data){

  data %<>% map(
    .f = ~ .x %>%
      ungroup() %>%
      mutate(._ID_. = 1:n())
  ) %>%
    bind_rows() %>%
    as_tibble() %>%
    arrange(._ID_.) %>%
    select(._ID_., everything())

  data

}


#' @rdname stack_data
#' @export
list_data <- function(data){

  n_impute <- length(data)

  data %<>%
    bind_rows(.id = '._ID_.') %>%
    as_tibble() %>%
    select(._ID_., everything())

  data

}



