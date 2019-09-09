#' one-hot coding
#'
#' @description Many modeling functions expect matrix input
#'   with factor levels one-hot encoded. `spread_cats`
#'   will one-hot encode any factor or character variable
#'   in `data` and return a one-hot encoded `tibble`.
#'   Alternatively, `gather_cats` will apply the inverse
#'   operation and convert one-hot encoded columns back
#'   into factors.
#'
#' @note factor levels are stored as an attribute
#'   in the output of `spread_cats`. If this attribute
#'   is intact when `gather_cats` is applied to the
#'   one-hot encoded data, it will not be necessary
#'   to specify `factor_levels`
#'
#' @param data data with categorical variables (i.e., factors)
#'   that need to be spread or gathered.
#' @param factor_levels This parameter is only relevant for
#'   `gather_cats`. A named list of factor levels, with each
#'    name corresponding to the column in the data that the
#'    factor levels describe.
#'
#' @inheritDotParams mltools::one_hot sparsifyNAs naCols dropCols
#'   dropUnusedLevels
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @export
#'
spread_cats <- function(data, factor_variables = NULL, ...){

  .dots <- list(...) %>%
    check_dots(
      valid_args =  c(
        'sparsifyNAs',
        'naCols',
        'dropCols',
        'dropUnusedLevels'
      )
    )

  data %<>% mutate_if(is.character, as.factor)

  factor_variables <- factor_variables %||% get_factors(data)

  .dots$dt <- as.data.table(data)
  .dots$cols <- factor_variables

  output <- do.call(one_hot, args = .dots)

  as_tibble(output)

}

#' @rdname spread_cats
#' @export
gather_cats <- function(
  data,
  factor_levels
) {

  old_names <- new_names <- names(data)

  factor_names <- factor_levels %>%
    enframe() %>%
    unnest(cols = value) %>%
    mutate(factor_name = paste(name, value, sep = "_")) %>%
    select(factor_name, name) %>%
    deframe()

  new_names <- unique(recode(old_names, !!!factor_names))

  for(i in seq_along(factor_levels)){

    .factor <- names(factor_levels)[i]
    .levels <- factor_levels[[i]]
    .names <- paste(.factor, .levels, sep = '_')

    new_col <- data[, .names] %>%
      apply(1, which.max) %>%
      as.numeric() %>%
      factor(levels = 1:length(.names), labels = .levels)

    data[[.factor]] <- new_col
    data[, .names] <- NULL

  }

  data[, new_names]

}
