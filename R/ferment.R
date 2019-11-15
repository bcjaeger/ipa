
#' Ferment a brew
#'
#' @description Missing values can occur in the training data,
#'   testing data, and validation data. An important requirement
#'   for missing value strategies is that only information from
#'   the training data should be used to impute missing data.
#'
#'   Unfortunately, some imputation strategies are not designed to
#'   work this way! For example, `softImpute` imputes missing
#'   values based on the index of the missing value in the training
#'   data, and this doesn't generalize to testing data because
#'   testing data (by definition) are not in the training data.
#'
#'   Fortunately, `ferment` gives you options. For example, you
#'   can use `softImpute` to impute missing training data, and then
#'   use `kneighbors` to impute the testing data by matching the
#'   testing observations to their nearest neighbors in the training
#'   data. More specifically, you can tell `ferment` to impute the
#'   testing data using either the original unimputed training data
#'   or using each of the imputed training datasets, separately.
#'   The same options apply for all `ipa_brew` objects.
#'
#' @param brew an `ipa_brew` object.
#'
#' @param ... Name-value pairs of datasets that will be imputed.
#'  The name of each argument will be the name of the variable
#'  in the `wort` of the `ipa_brew` that comprises imputed
#'  datasets. The value of each argument will be a dataset that
#'  is imputed and then placed into the designated column in
#'  the `wort`.
#'
#' @param dbl_impute (`TRUE`/`FALSE`). If `TRUE`, then imputed
#'   training data will be used to impute missing values in
#'   `new_data`. Otherwise, the original `data` (i.e., the data
#'   containing missing values) will be used to impute missing
#'   values in `new_data`.
#'
#' @param dbl_neighbors an integer value specifying the number of
#'   nearest neighbors to identify when imputing testing data.
#'   This argument only applies when the given `ipa_brew` does
#'   not have a known method to impute new data. If you are
#'   making a `kneighbors` brew, this argument is only relevant
#'   when `dbl_impute = FALSE`. Otherwise, the sequence of neighbors
#'   used to create imputed training sets will be copied when imputing
#'   testing data.
#'
#' @note What is a `wort`? A component of a `brew` object that
#'   contains imputed datasets, models used to impute those datasets,
#'   and the corresponding hyper-parameters of those models.
#'
#' @export
#'
#' @examples
#'
#' data <- data.frame(
#'   x1 = 1:10,
#'   x2 = 10:1,
#'   x3 = rnorm(10),
#'   outcome = 11 + runif(10)
#' )
#'
#' data[1:2, 1] = NA
#' data[5:6, 2] = NA
#'
#' new_data = data.frame(
#'   x1 = 1:10,
#'   x2 = 10:1,
#'   x3 = rnorm(10),
#'   outcome = 11 + runif(10)
#' )
#'
#' new_data[6:7, 1] = NA
#' new_data[2:3, 2] = NA
#'
#' knn_brew <- brew(data, outcome = outcome, flavor = 'kneighbors')
#'
#' spicy_knn <- spice(
#'   brew = knn_brew,
#'   neighbors = c(3, 5),
#'   aggr_neighbors = c(TRUE, FALSE)
#' )
#'
#' mashed_knn <- mash(spicy_knn)
#'
#' fermented_knn <- ferment(mashed_knn, new_data = new_data)
#'
#' set.seed(101)
#' n=200
#' p=100
#' J=50
#' np=n*p
#' missfrac=0.3
#' x=matrix(rnorm(n*J),n,J)%*%matrix(rnorm(J*p),J,p)+matrix(rnorm(np),n,p)/5
#' ix=seq(np)
#' imiss=sample(ix,np*missfrac,replace=FALSE)
#' xna=x
#' xna[imiss]=NA
#'
#' data <- as.data.frame(xna)
#'
#' soft_brew <- brew(data, outcome = c(V1,V2), flavor = 'softImpute')
#' soft_brew <- spice(soft_brew, n_impute = 10, step_size = 2)
#' soft_brew <- mash(soft_brew, scale_lambda = 0.12)
#'
#'

ferment <- function(
  brew,
  ...,
  dbl_impute = FALSE,
  dbl_neighbors = 5
) {

  UseMethod('ferment')

}

ferment_nbrs <- function(
  brew,
  new_data,
  dbl_impute,
  neighbor_sequence,
  neighbor_aggregate,
  fermented_cols
){

  verbose <- get_verbosity(brew)
  verbose_1 <- verbose >= 1
  verbose_2 <- verbose >= 2

  new_names <- names(new_data)

  if(purrr::is_empty(new_data)){

    if(verbose_1)
      message("No new data supplied, returning imputed training sets")

    attr(brew, 'fermented') <- TRUE
    attr(brew, 'fermented_cols') <- fermented_cols
    return(brew)

  }

  if(any("" %in% new_names) || is.null(new_names))
    stop("all new data sets must be named", call. = FALSE)

  # if new_data is supplied
  outcome <- attr(brew, 'outcome')$name


  # take outcome columns out of new data
  # save the outcome columns in the brew
  for(i in seq_along(new_data)){

    if( any( outcome %in% names(new_data[[i]]) ) ) {

      attr(brew, 'outcome')$data[[ new_names[i] ]] <-
        new_data[[i]][, outcome, drop = FALSE]

      new_data[[i]][, outcome] = NULL

    }

    if( !any( is.na(new_data[[i]]) ) ){

      brew$wort[[ new_names[i] ]] <- list( new_data[[i]] )

      if(verbose_1)
        message(
          glue::glue(
            "No missing values in {new_names[i]} - nothing to impute."
          )
        )

      fermented_cols %<>% c( new_names[i] )

    }

  }


  data_to_impute <- c('training', new_names) %>%
    setdiff(fermented_cols)

  no_missing_vals <- purrr::is_empty(data_to_impute)

  if(no_missing_vals){
    attr(brew, 'fermented') <- TRUE
    attr(brew, 'fermented_cols') <- fermented_cols
    return(brew)
  }

  # if new data have missing values

  for(i in data_to_impute){

    if(verbose_1)
      message(glue::glue(
        "Fitting models to impute missing values in",
        " {i} using nearest neighbors")
      )

    check_ferment_data(brew, new_data[[i]])

    if(dbl_impute){

      imputes <- vector(mode = 'list', length = brew$pars$n_impute)

      if( length(neighbor_sequence) == 1 )
        neighbor_sequence <- rep(neighbor_sequence, length(imputes))

      for(impute_index in seq_along(imputes)){

        if(verbose_1)
          message(glue::glue(
            "Identifying nearest neighbors ",
            "using dataset {impute_index}")
          )

        nn_index <- gower_topn(
          x = new_data[[i]],
          y = brew$wort$training[[impute_index]],
          n = neighbor_sequence[impute_index]
        )$index

        imputes[[impute_index]] <- nn_impute(
          ref_data = brew$wort$training[[impute_index]],
          new_data = new_data[[i]],
          nn_index = nn_index,
          neighbors = neighbor_sequence[impute_index]
        )

      }

      brew$wort[[i]] <- imputes

    } else {

      if(verbose_2)
        message(glue::glue(
          "Note: Only the original training data ",
          "are being used to impute values in {i}")
        )

      n_impute <- brew$pars$n_impute

      if(length(neighbor_sequence) == 1)
        neighbor_sequence <- rep(neighbor_sequence, n_impute)

      if(length(neighbor_aggregate) == 1)
        neighbor_aggregate <- rep(neighbor_aggregate, n_impute)

      if(length(neighbor_sequence) != n_impute){
        stop(glue::glue(
          "The sequence of neighborhood sizes is length \\
          {length(neighbor_sequence)} but the number of imputed \\
          datasets is {n_impute}."),
          call. = FALSE)
      }

      if(length(neighbor_aggregate) != n_impute){
        stop(glue::glue(
          "The sequence of neighborhood aggregate values is length \\
          {length(neighbor_aggregate)} but the number of imputed \\
          datasets is {n_impute}."),
          call. = FALSE)
      }

      imputes <- knn_work(
        ref_data = brew$data,
        new_data = new_data[[i]],
        n_impute = brew$pars$n_impute,
        neighbor_sequence = neighbor_sequence,
        neighbor_aggregate = neighbor_aggregate,
        verbose = verbose
      )

      brew$wort[[i]] <- imputes

    }

    fermented_cols %<>% c(i)

  }

  attr(brew, 'fermented') <- TRUE
  attr(brew, 'fermented_cols') <- fermented_cols

  brew

}


#' @describeIn ferment impute missing values for `softImpute_brew` objects.
#' @export

ferment.softImpute_brew <- function(
  brew,
  ...,
  dbl_impute = FALSE,
  dbl_neighbors = 5
) {

  check_brew(brew, expected_stage = 'ferment')
  check_pos_int(dbl_neighbors, label = 'number of neighbors')

  # Impute training data using softImpute
  brew$wort %<>% dplyr::mutate(
    training = purrr::map(
      .x = fit,
      .f = ~ brew$data %>%
        softImpute::complete(.x, unscale = attr(brew, 'unscale')) %>%
        tibble::as_tibble()
    )
  )

  ferment_nbrs(
    brew = brew,
    new_data = list(...),
    dbl_impute = dbl_impute,
    neighbor_sequence = dbl_neighbors,
    neighbor_aggregate = TRUE,
    fermented_cols = "training"
  )

}


#' @describeIn ferment impute missing values for `kneighbors_brew` objects.
#' @export

ferment.kneighbors_brew <- function(
  brew,
  ...,
  dbl_impute = FALSE,
  dbl_neighbors = 5
) {

  check_brew(brew, expected_stage = 'ferment')
  check_pos_int(dbl_neighbors, label = 'number of neighbors')

  # impute training data
  brew$wort %<>% dplyr::mutate(training = fit)

  ferment_nbrs(
    brew = brew,
    new_data = list(...),
    dbl_impute = dbl_impute,
    neighbor_sequence = brew$pars$nbrs,
    neighbor_aggregate = brew$pars$aggr,
    fermented_cols = "training"
  )

}

#' @describeIn ferment impute missing values for `missRanger_brew` objects.
#' @export

ferment.missRanger_brew <- function(
  brew,
  ...,
  dbl_impute = FALSE,
  dbl_neighbors = 5
){

  check_brew(brew, expected_stage = 'ferment')
  check_pos_int(dbl_neighbors, label = 'number of neighbors')

  # impute training data
  brew$wort %<>% dplyr::mutate(training = fit)

  ferment_nbrs(
    brew = brew,
    new_data = list(...),
    dbl_impute = dbl_impute,
    neighbor_sequence = dbl_neighbors,
    neighbor_aggregate = TRUE,
    fermented_cols = "training"
  )

}

#' @rdname ferment
#' @export

is_fermented <- function(brew){
  attr(brew, 'fermented')
}



