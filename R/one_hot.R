


#' One hot encoding
#'
#' A faster implementation of [mltools::one_hot] with less options.
#'
#' @param data a data frame
#'
#' @return a `data.table` with one-hot encoded factors.
#'
#' @note One-hot-encoding converts an unordered categorical vector
#'   (i.e. a factor) to multiple binarized vectors where each binary
#'   vector of 1s and 0s indicates the presence of a class (i.e. level)
#'   of the of the original vector.
#'
#' @export
#'
#' @examples
#' n <- 10
#'
#' data <- data.frame(
#'   V1 = seq(n),
#'   V2 = factor(sample(letters[1:3], n, replace = TRUE)),
#'   V3 = seq(n) / 10,
#'   V4 = factor(sample(letters[5:6], n, replace = TRUE))
#' )
#'
#' data$V1[1] <- NA
#' data$V3[c(6,7)] <- NA
#' data$V2[1:2] <- NA
#' data$V4[2] <- NA
#'
#' one_hot(data)

one_hot <- function (data){

  output_fun <- switch (class(data)[1],
    'data.frame' = as.data.frame,
    'matrix' = as.matrix,
    'tbl_df' = tibble::as_tibble,
    'data.table' = function(x) x,
    stop("unrecognized type for data", call. = FALSE)
  )

  if(!is.data.table(data)){
    DT <- as.data.table(data)
  } else {
    DT <- copy(data)
  }

  if(any(sapply(DT, is.character))){

    chr_cols <- names(DT)[sapply(DT, is.character)]

    for(col in chr_cols)
      data.table::set(DT, j = col, value = as.factor(DT[[col]]))

  }

  # Will use these original names to help re-order the output
  DT_names <- names(DT)

  # will use the factor info about DT to connect
  # one-hot columns to original factors
  fctr_info <- get_factor_info(DT)

  for(i in seq_along(fctr_info$cols)){

    # the idea is to make a matrix for each factor
    # with nrow = nrow(DT) and ncol = length of factor levels.
    mat <- matrix(0,
      nrow = nrow(DT),
      ncol = length(fctr_info$lvls[[i]])
    )

    colnames(mat) <- fctr_info$keys[[i]]

    # missing values of the factor become missing rows
    mat[is.na(DT[[fctr_info$cols[i]]]), ] <- NA_integer_

    # we will one-hot encode the matrix and then bind it to DT,
    # replacing the original factor column. Go through the matrix
    # column by column, where each column corresponds to a level
    # of the current factor (indexed by i). Flip the values
    # of the j'th column to 1 whenever the current factor's value
    # is the j'th level.

    for (j in seq(ncol(mat))) {

      # find which rows to turn into 1's. These should be the
      # indices in the currect factor where it's value is equal
      # to the j'th level.
      hot_rows <- which(
        DT[[fctr_info$cols[i]]] == fctr_info$lvls[[i]][j]
      )

      # after finding the rows, flip the values from 0 to 1
      if(!purrr::is_empty(hot_rows)){
        mat[hot_rows , j] <- 1
      }

    }


    DT[, fctr_info$cols[i]] <- NULL

    DT <- cbind(DT, mat)

  }

  OH_names <- DT_names

  for (i in seq_along(fctr_info$cols)){

    OH_names <- insert_vals(
      vec = OH_names,
      where = which(fctr_info$cols[i] == OH_names),
      what = fctr_info$keys[[i]]
    )

  }

  data.table::setcolorder(DT, OH_names)

  output_fun(DT)

}

one_hot_vec <- function(x, ncats){

  x <- x + 1

  mat <- matrix(0, ncol = ncats, nrow = length(x))

  for(i in seq(ncats)) mat[x==i, i] <- 1

  mat

}

insert_vals <- function(vec, where, what){

  stopifnot(
    typeof(what) == typeof(vec),
    where >= 1 & where <= length(vec)
  )

  if(where == 1){

    if(length(vec) == 1) return(c(what)) else return(c(what, vec[-1]))
  }

  if(where == length(vec)) return(c(vec[1:(length(vec)-1)], what))

  vec_left <- vec[1:(where-1)]
  vec_right <- vec[(where+1):length(vec)]

  c(vec_left, what, vec_right)

}

one_hot_chr <- function(x, lvls){

  mt <- matrix(0, nrow = length(x), ncol = length(lvls))

  for(i in seq_along(lvls)){

    indx <- which(x == lvls[i])
    if(!is_empty(indx)) mt[indx, i] <- 1

  }

  mt

}


