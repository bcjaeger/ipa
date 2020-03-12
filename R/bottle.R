

#' Bottle a brew
#'
#' @param brew an `ipa_brew` object.
#'
#' @param type a character value indicating what composition
#'   the training and testing data should have. Valid options
#'   are 'tibble' and 'matrix'
#'
#' @param drop_fit a logical value. If `TRUE`, the column in
#'   the `wort` comprising imputation models will be dropped.
#'   Otherwise, the column will be retained.
#'
#' @export
#'
#' @examples
#'
#' x1 = rnorm(100)
#' x2 = rnorm(100) + x1
#' x3 = rnorm(100) + x1 + x2
#'
#' outcome = 0.5 * (x1 - x2 + x3)
#'
#' n_miss = 10
#' x1[1:n_miss] <- NA
#'
#' data <- data.frame(x1=x1, x2=x2, x3=x3, outcome=outcome)
#'
#' sft_brew <- brew_soft(data, outcome=outcome, bind_miss = FALSE)
#' sft_brew <- mash(sft_brew, with = masher_soft(bs = TRUE))
#' sft_brew <- stir(sft_brew, timer = TRUE)
#'
#' data_new = data.frame(
#'   x1      = c(1/2, NA_real_),
#'   x2      = c(NA_real_, 2/3),
#'   x3      = c(5/2, 2/3),
#'   outcome = c(1/3, 2/3)
#' )
#'
#' # soft models are re-fitted after stacking data_new with data_ref
#'
#' sft_brew <- ferment(sft_brew, data_new = data_new)
#'
#' bottle(sft_brew, type = 'tibble')
#'
#'
# TODO: there is a problem with modify in place. document or change?
bottle <- function(
  brew,
  type = 'tibble',
  drop_fit = TRUE
) {

  check_brew(brew, expected_stage = 'bottle')
  check_chr(type, label = 'type', options = c('tibble', 'matrix'))
  check_l1_stop(type, label = 'type')
  check_bool(drop_fit, label = 'drop_fit')
  check_l1_stop(drop_fit, label = 'drop_fit')

  .cols <- attr(brew, 'fermented_cols')

  for(.col in .cols){

    .col_name <- paste('iv', .col, sep = '_')

    # fill in the training/testing data
    set(brew$wort, j = .col_name,
      value = purrr::map(
        .x = brew$wort[[.col_name]],
        .f = ~fill_na(brew$data[[.col]], .x)
      )
    )

    # bind outcome column to the imputed datasets
    set(brew$wort, j = .col_name,
      value = purrr::map(
        .x = brew$wort[[.col_name]],
        .f = ~ cbind(attr(brew, 'outcome')[[.col]], .x)
      )
    )

  }

  setnames(brew$wort,
    paste0("iv_", attr(brew, 'fermented_cols')),
    attr(brew, 'fermented_cols')
  )

  if(drop_fit & 'fit' %in% names(brew$wort)){
    brew$wort$fit <- NULL
  }

  par_cols <- switch (get_flavor(brew),
    softImpute = c('lambda', 'rank_max', 'rank_fit'),
    kneighbors = c('k_neighbors')
  )

  brew$wort <- set(brew$wort, j = 'pars',
    value = apply(brew$wort[, ..par_cols], 1, as.list))

  brew$wort[, (par_cols) := NULL]

  setcolorder(brew$wort, c('impute', 'pars'))

  brew <- switch (
    EXPR = type,
    'tibble' = .tibble_bottle(brew),
    'matrix' = .matrix_bottle(brew)
  )

  brew

}


.tibble_bottle <- function(brew){


  .cols <- attr(brew, 'fermented_cols')

  for(.col in .cols){

    set(brew$wort, j = .col,
      value = list(purrr::map(brew$wort[[.col]], tibble::as_tibble)))

  }

  # this NEEDS to be a tibble.
  # data.tables don't do well with columns that are long lists
  # brew$wort <- tibble::as_tibble(brew$wort)

  attr(brew, 'bottled') <- TRUE
  attr(brew, 'composition') <- 'tibble'

  brew

}

.matrix_bottle <- function(brew){

  .cols <- attr(brew, 'fermented_cols')

  xvar <- names(brew$data$training)
  outcome <- get_outcome_name(brew)

  for(col in .cols){

    brew$wort[[col]] <- purrr::map(brew$wort[[col]], .f = ~ {
      list(Y = .x[[outcome]], X = as.matrix(one_hot(.x[, ..xvar])))
    })

  }

  attr(brew, 'bottled') <- TRUE
  attr(brew, 'composition') <- 'matrix'

  brew

}


