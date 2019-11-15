

#' Data Stacks & Lists
#'
#' @description stacked dataframes comprise multiply imputed
#'   dataframes that are stacked on top of one another.
#'   A single models can be fit to stacked data, and the
#'   additional diversity in the data can sometimes reduce
#'   the model's prediction error. Data lists are similar,
#'   to data stacks, but data frames in a list are kept
#'   separate and one model is fit for each dataframe.
#'
#'   These functions create ID variables and attach them
#'   to the output data.
#'
#' @param data (`list`) This argument should be a list of
#'   imputed dataframes, each having the same number of rows and columns.
#'
#' @param keep_ID (`TRUE / FALSE`) Should the returned object include
#'   a column named `._ID_.` that indicates which observations in the
#'   stacked/listed imputed data belong to the same observation in the
#'   original unimputed data?
#'
#' @export
#'
#' @examples
#' data_list <- list(
#'   df1 = data.frame(x = 1:3, y = letters[1:3], stringsAsFactors=FALSE),
#'   df2 = data.frame(x = 2:4, y = letters[1:3], stringsAsFactors=FALSE)
#' )
#'
#' stack_data(data_list, keep_ID = TRUE)
#'
#' list_data(data_list, keep_ID = TRUE)

stack_data <- function(data, keep_ID = FALSE){

  check_data_list(data)

  data %<>% purrr::map(
    .f = ~ .x %>%
      dplyr::ungroup() %>%
      dplyr::mutate(._ID_. = seq(dplyr::n()))
  ) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble() %>%
    dplyr::arrange(._ID_.) %>%
    dplyr::select(._ID_., dplyr::everything())

  if(keep_ID) return(data)

  dplyr::select(data, -._ID_.)

}


#' @rdname stack_data
#' @export
list_data <- function(data, keep_ID = FALSE){

  check_data_list(data)

  data %<>%
    purrr::map(
      .f = dplyr::mutate,
      ._ID_. = seq(dplyr::n())
    ) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble() %>%
    dplyr::select(._ID_., dplyr::everything())

  if(keep_ID) return(data)

  dplyr::select(data, -._ID_.)

}



