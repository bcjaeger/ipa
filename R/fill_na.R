



#' Fill missing values
#'
#' @param data a data frame with missing values
#' @param vals a named list. Names correspond to columns in `data` with
#'   missing values.
#' @param make_new_DT a logical value that is only relevant if `data`
#'   is a `data.table`. if `TRUE`, a new object is created and `data`
#'   will not be modified. If `FALSE`, `data` will be modified in place.
#'   The latter will be more efficient but is onyl feasible when you
#'   only need to make one imputed data frame.
#'
#' @return a data frame with missing values filled in.
#'
#' @export
#'
#' @examples
#'
#' df = data.frame(A = 1:5, B = letters[1:5])
#' df[1, 1] = NA
#' df[2, 2] = NA
#'
#' vals = list(A = 111, B = 'bbb')
#' fill_na(df, vals)
#'

fill_na <- function(data, vals, make_new_DT = TRUE){
  UseMethod("fill_na")
}

#' @export
fill_na.default <- function(data, vals, make_new_DT = TRUE){
  stop("unrecognized data type: <", class(data)[1], "> \n",
    " supported types are <data.table>, <data.frame>, and <tibble>.",
    call. = FALSE)
}

#' @export
fill_na.data.frame <- function(data, vals, make_new_DT = TRUE){

  as.data.table(data) %>%
    fill_na(vals, make_new_DT = FALSE) %>%
    as.data.frame()

}

#' @export
fill_na.data.table <- function(data, vals, make_new_DT = TRUE){

  if (make_new_DT) {
    # create a deep copy of the input
    new_dt <- copy(data)
  } else {
    # or modify the input in place...
    new_dt <- data
  }

  val_names <- names(vals)
  bad_names <- setdiff(names(vals), names(data))

  if(!purrr::is_empty(bad_names)) stop("vals contains variable names ",
    "that are not in data: ", list_things(bad_names), call. = FALSE)

  na_indx <- lapply(new_dt, function(x) which(is.na(x)))
  no_miss <- names(which(sapply(na_indx, function(x) length(x)==0)))
  na_indx[no_miss] <- NULL

  # when softImpute is used and the imputed values are not restored
  # to their original type, data.table will get upset by you trying
  # to put doubles into integer columns. Since this is something we
  # actually want to do when using restore = FALSE with soft_impute,
  # we'll coerce the values to a double.
  val_types <- sapply(vals, typeof)

  dt_types <- sapply(new_dt[, ..val_names], typeof)
  dt_convert <- names(which(val_types == 'double' & dt_types == 'integer'))

  if(!purrr::is_empty(dt_convert)){

    for(i in dt_convert){
      new_dt[[i]] <- as.double(new_dt[[i]])
    }

  }

  for(col in names(vals)){

    if(!(col %in% names(na_indx))){
      stop(glue::glue(
        "data${col} does not have missing values, but vals${col}",
        " contains {length(vals[[col]])} values.",
        " Is data correctly specified?"),
        call. = FALSE)
    }

    if(length(na_indx[[col]]) != length(vals[[col]])){
      stop(glue::glue(
        "data${col} has {length(na_indx[[col]])} missing values",
        " but vals${col} has {length(vals[[col]])} values"),
        call. = FALSE)
    }

    set(new_dt, i = na_indx[[col]], j = col, value = vals[[col]])

  }

  new_dt

}

#' @export
fill_na.tbl_df <- function(data, vals, make_new_DT = TRUE){

  val_names <- names(vals)
  bad_names <- setdiff(names(vals), names(data))

  if(!purrr::is_empty(bad_names)) stop("vals contains variable names ",
    "that are not in data: ", list_things(bad_names), call. = FALSE)

  na_indx <- lapply(data, function(x) which(is.na(x)))
  no_miss <- names(which(sapply(na_indx, function(x) length(x)==0)))
  na_indx[no_miss] <- NULL

  for(col in names(vals)){

    if(!(col %in% names(na_indx))){
      stop(glue::glue(
        "data${col} does not have missing values, but vals${col}",
        " contains {length(vals[[col]])} values.",
        "\nIs data correctly specified?"
      ))
    }

    if(length(na_indx[[col]]) != length(vals[[col]])){
      stop(glue::glue(
        "data${col} has {length(na_indx[[col]])} missing values",
        " but vals${col} has {length(vals[[col]])} values"
      ), call. = FALSE)
    }

    data[na_indx[[col]], col] <- vals[[col]]

  }

  data


}


