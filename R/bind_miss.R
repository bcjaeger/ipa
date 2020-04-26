


#' Missing index
#'
#' @param data a data set with missing values.
#' @param drop_empty a logical value. If `TRUE`, columns in `data` without any
#'   missing values will be dropped from the output. If `FALSE`, all column
#'   names in `data` will be present in the output.
#'
#' @return a list with indices of missing values in `data`.
#'
#' @export
#'
#' @examples
#'
#' dat <- data.frame(a = c(1, NA), b = c(NA, 2), c = c(1,2))
#'
#' mindx(dat)
#' mindx(dat, drop_empty = FALSE)
#'
#'
mindx <- function(data, drop_empty = TRUE){

  out <- purrr::map(data, ~which(is.na(.x)))

  if(drop_empty) drop_empty(out) else out

}

as_indicator <- function(indx, length){

  vec <- vector(mode = 'integer', length = length)

  if(is.null(indx)) return(vec)

  if (!purrr::is_empty(indx)) vec[indx] <- 1L

  vec

}

.bind_miss <- function(DT,
  miss_indx = NULL, cols = NULL,
  sep = '_', miss_chr = 'missing'
){

  drop_const <- is.null(cols)
  # get missing indices for DT
  miss_indx <- miss_indx %||% mindx(DT, drop_empty = TRUE)
  # use all columns if nothing was specified
  cols <- cols %||% names(miss_indx)

  N <- nrow(DT)
  old_names <- copy(names(DT))
  new_names <- purrr::set_names(paste0(cols, sep, miss_chr), cols)

  for(i in cols)
    set(DT, j = new_names[i], value = as_indicator(miss_indx[[i]], N))

  if(drop_const && N > 1){

    dropper <- function(x) (1 %in% x) && (0 %in% x)

    keep <- DT[, lapply(.SD, dropper), .SDcols = new_names] %>%
      unlist() %>%
      which() %>%
      names()

  } else {

    keep <- new_names

  }

  keep <- c(old_names, keep)

  DT[, ..keep]

}
