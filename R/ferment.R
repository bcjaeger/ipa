#' Ferment a brew
#'
#' @description Missing values can occur in training data and
#'   testing data.
#'
#'   Unfortunately, some imputation strategies are only designed
#'   to impute missing training data. For example, `softImpute`
#'   imputes missing values based on the index of the missing
#'   value in the training data, and this doesn't generalize
#'   to testing data because testing data (by definition)
#'   do not have indices in the training data.
#'
#'   `ferment` generally adheres to the principle of using only
#'   training data to impute missing testing data, except when it
#'   can't (i.e., when `flavor = 'softImpute'`).
#'
#'   `ferment` automatically copies the data-processing and imputation
#'   arguments used in previous brewing steps. Specifically, if `brew`
#'   was called with `bind_miss = TRUE`, then the missing value indicator
#'   matrix for `data_new` will be bound to `data_new` and used in the
#'   imputation procedure. Additionally, imputation parameters specified
#'   in the `spice` and `mash` steps will automatically be implemented
#'   in the `ferment` step.
#'
#' @param brew an `ipa_brew` object.
#'
#' @param data_new a data frame with missing values.
#'
#' @examples
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
#'
#' ferment(sft_brew)
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
#' ferment(sft_brew, data_new = data_new)
#'
#'
#' @note What is a `wort`? A component of a `brew` object that
#'   contains imputed datasets, models used to impute those datasets,
#'   and the corresponding hyper-parameters of those models.
#'
#' @export

ferment <- function(brew, data_new = NULL){

  # brew should be spiced and mashed by now
  check_brew(brew, expected_stage = 'ferment')

  # fill in the training data
  set(brew$wort,
    j = 'training',
    value = purrr::map(
      .x = brew$wort$imputed_values,
      .f = ~fill_na(brew$data$training, .x)
    )
  )

  brew$wort$imputed_values <- NULL
  attr(brew, 'fermented_cols') <- "training"

  # if there aren't any testing data, stop here
  if (is.null(data_new)) {

    attr(brew, 'fermented') <- TRUE
    return(brew)

  }

  setDT(data_new)

  # outcome column name
  outcome <- get_outcome(brew)$name

  # pull the outcome out of testing data and attach
  # it to the brew as an attribute if necessary.
  if(outcome %in% names(data_new)){

    if (any(is.na(data_new[, ..outcome]))) stop(
      glue::glue("missing values in outcome columns ",
        "({list_things(outcome)}) are not allowed."),
      call. = FALSE
    )

    attr(brew, 'outcome')$data$testing <- data_new[, ..outcome]
    data_new[[outcome]] <- NULL

  }

  # bind missing cols to data_new if needed
  if(get_bind_miss(brew)){
    # using drop_const = FALSE to make sure all columns are included
    miss_new <- mindx(data_new, drop_const = FALSE)
    # then filtering down to only the columns that are in training data
    miss_new <- miss_new[, names(brew$miss$training)]
    # this forces the training/testing sets to be stackable.
    brew$miss$testing <- miss_new
  }

  # update the brew's data to include both the
  # training set and testing set.
  brew$data$testing <- data_new

  # check testing data names and types
  # (they need to be equal to those of brew$data)
  check_data_new_names(
    data_ref = brew$data$training,
    data_new = data_new
  )

  check_data_new_types(
    data_ref = brew$data$training,
    data_new = data_new
  )

  impute_args <- brew$pars
  impute_args$data_ref <- brew$data$training
  impute_args$data_new <- data_new

  if(get_bind_miss(brew)){

    impute_args$data_ref <- impute_args$data_ref %>%
      dplyr::bind_cols(brew$miss$training)

    impute_args$data_new <- impute_args$data_new %>%
      dplyr::bind_cols(brew$miss$testing)

  }

  brew$wort$testing <- do.call(
    what = switch(
      get_flavor(brew),
      'kneighbors' = impute_nbrs,
      'softImpute' = impute_soft
    ),
    args = impute_args
  )$imputed_values

  # fill in the testing data
  set(brew$wort,
    j = 'testing',
    value = purrr::map(
      .x = brew$wort$testing,
      .f = ~fill_na(brew$data$testing, .x)
    )
  )

  attr(brew, 'fermented_cols') <- c(attr(brew, 'fermented_cols'), "testing")
  attr(brew, 'fermented') <- TRUE

  brew

}

