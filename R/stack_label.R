
#' Stack labels
#'
#' @description
#'
#' When `xgboost` is applied to lists of datasets or stacked data
#'   (i.e., multiply imputed data ),
#'   the label vector must be adjusted so that it is consistent with
#'   the structure of the x matrix (or matrices). This function
#'   is handy for expanding label vectors in this manner.
#'
#' @details
#'
#' `stack_label()`can be used to create vectors of labels, and
#'   `list_label()` creates lists of vectors of labels.
#'
#' @param label A vector of label values. For continuous outcomes, the
#'   vector should have numeric values. For categorical outcomes, the
#'   vector should be a factor. For survival outcomes, the vector
#'   should be numeric and contain values indicating time until
#'   the event or censoring (censored values should be negative).
#' @param n_impute An integer indicating the number of imputed datasets.
#'
#' @examples
#'
#' y <- c(1,2,3)
#'
#' stack_label(y, n_impute = 2)
#' list_label(y, n_impute = 2)
#'
#' @export
#'


#' @rdname labels_expand
#' @export
stack_label <- function(label, n_impute){
  UseMethod('stack_label')
}

#' @rdname labels_expand
#' @export
list_label <- function(label, n_impute){
  UseMethod('list_label')
}

#' @export
stack_label.default <- function(label, n_impute){

  rep(label, each = n_impute) %>%
    add_class_last("stacked_label")

}

#' @export
stack_label.list <- function(label, n_impute){

  map(label, rep, each = n_impute) %>%
    map(add_class_last, "stacked_label")

}

#' @export
list_label.default <- function(label, n_impute){

  rerun(n_impute, label) %>%
    add_class_last("mi_label")

}

#' @export
list_label.list <- function(label, n_impute){

  new_label <- vector(mode='list', length = length(label))

  for(i in seq_along(new_label)){

    new_label[[i]] <- rerun(n_impute, label) %>%
      add_class_last("mi_label")

  }

  set_names(new_label, names(label))

}




