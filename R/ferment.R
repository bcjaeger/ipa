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
#' @inheritParams stir
#'
#' @param data_new a data frame with missing values.
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

ferment <- function(brew, data_new = NULL, timer = FALSE){

  # brew should be spiced and mashed by now
  check_brew(brew, expected_stage = 'ferment')

  attr(brew, 'fermented_cols') <- "training"

  # if there aren't any testing data, stop here
  if (is.null(data_new)) {

    attr(brew, 'fermented') <- TRUE
    return(brew)

  }

  if(!is.data.table(data_new))
    DT_new <- copy(as.data.table(data_new))
  else
    DT_new <- copy(data_new)

  # outcome column name
  outcome <- get_outcome_name(brew)

  # pull the outcome out of testing data and attach
  # it to the brew as an attribute if necessary.
  if(any(outcome %in% names(DT_new))){

    if (any(is.na(DT_new[, ..outcome]))) stop(
      glue::glue("missing values in outcome columns ",
        "({list_things(outcome)}) are not allowed."),
      call. = FALSE
    )

    attr(brew, 'outcome')$testing <- copy(DT_new[, ..outcome])

    # remove all outcomes from DT
    DT_new <- DT_new[, (outcome) := NULL]

  }

  brew$miss$testing <- mindx(DT_new, drop_empty = TRUE)

  if(get_bind_miss(brew))
    DT_new <- .bind_miss(DT_new,
      miss_indx = brew$miss$testing,
      cols = names(brew$miss$training))

  # check testing data names and types
  # (they need to be equal to those of brew$data)
  check_data_new_names(
    data_ref = brew$data$training,
    data_new = DT_new
  )

  check_data_new_types(
    data_ref = brew$data$training,
    data_new = DT_new
  )

  # update the brew's data to include both the
  # training set and testing set.
  brew$data$testing <- DT_new

  impute_args <- brew$pars
  impute_args$data_ref <- brew$data$training
  impute_args$data_new <- brew$data$testing

  if(timer){

    start <- Sys.time()

    if(get_verbosity(brew) > 0){

      .flavor <- switch(get_flavor(brew),
        'kneighbors' = 'k-nearest-neighbors',
        'softImpute' = 'soft imputation')

      message("Fitting ", .flavor, " models to testing data...")

    }

  }

  brew$wort$iv_testing <- do.call(
    what = switch(get_flavor(brew),
      'kneighbors' = impute_nbrs,
      'softImpute' = impute_soft),
    args = impute_args)$imputed_values

  if(timer){

    stop <- Sys.time()
    dt_val <- as.difftime(stop-start)
    attr(brew, 'ferment_time') <- dt_val

    if(get_verbosity(brew) > 0){
      dt_msg <- paste(round(dt_val, 2), attr(dt_val, 'units'))
      message("Finished after ", dt_msg)
    }

  }

  attr(brew, 'fermented_cols') <- c(attr(brew, 'fermented_cols'), "testing")
  attr(brew, 'fermented') <- TRUE

  brew

}

