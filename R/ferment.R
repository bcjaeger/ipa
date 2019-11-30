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
#' @param ... Name-value pairs of expressions. The name of each
#'  argument will be the name of a new variable in the `wort`
#'  of the `ipa_brew` object. The value of each argument should be
#'  an object created using the [test_nbrs] or [test_stkr] functions.
#'  The inputs of these functions will determine how testing data
#'  are imputed.
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
#' knn_spiced <- spice(
#'   brew = knn_brew,
#'   neighbors = c(3, 5),
#'   aggr_neighbors = c(TRUE, FALSE)
#' )
#'
#' knn_mashed <- mash(knn_spiced)
#'
#' # use test_nbrs to impute new data with nearest neighbors
#' knn_fermed <- ferment(knn_mashed, test = test_nbrs(new_data))
#'
#' # new column called 'test' is in the wort now
#' knn_fermed$wort
#'
#'


ferment <- function(brew, ...){

  # brew should be ready for spiced and mashed by now
  check_brew(brew, expected_stage = 'ferment')

  # timpers = list of test_imputers
  timpers   <- purrr::map(list(...), check_test_imputer)
  new_data  <- purrr::map(timpers, 'data')
  new_names <- names(new_data)
  # verbosity determines printed output
  verbose   <- get_verbosity(brew)
  verbose_1 <- verbose >= 1
  verbose_2 <- verbose >= 2

  # impute training data
  if(verbose_1) message("imputing training data")
  brew %<>% impute_train()
  attr(brew, 'fermented_cols') <- "training"

  if(purrr::is_empty(new_data)){
    if(verbose_1)
      message("No new data supplied, returning imputed training sets")
    attr(brew, 'fermented') <- TRUE
    return(brew)
  }

  if(any(c(""," ") %in% new_names) || is.null(new_names)){
    stop("Inputs should be name-value pairs.\n",
      "For example, test_data = test_nbrs(data = your_data) ",
      "is a valid input.\n",
      "the name (i.e., test_data) will be the name of a new ",
      "column in the wort",
      call. = FALSE)
  }

  outcome <- attr(brew, 'outcome')$name

  for(i in seq_along(new_data)){

    if( any( outcome %in% names(new_data[[i]]) ) ) {

      attr(brew, 'outcome')$data[[ new_names[i] ]] <-
        new_data[[i]][, outcome, drop = FALSE]

    }
  }

  # checks are done - we don't need this list anymore
  rm(new_data)

  for(i in seq_along(timpers)){

    timpers[[i]]$data[, outcome] = NULL

    if(timpers[[i]]$strat == 'nbrs'){

      neighbor_sequence <-
        timpers[[i]]$neighbors %||%
        brew$pars$nbrs %||%
        5L

      neighbor_aggregate <-
        timpers[[i]]$aggr_neighbors %||%
        brew$pars$aggr %||%
        TRUE

      brew %<>% ferment_nbrs(
        new_data = timpers[[i]]$data,
        new_name = names(timpers)[i],
        dbl_impute = timpers[[i]]$dbl_impute %||% FALSE,
        neighbor_sequence = neighbor_sequence,
        neighbor_aggregate = neighbor_aggregate
      )

    }

    if(timpers[[i]]$strat == 'stack'){

      stkr_fun <- switch(
        attr(brew, 'flavor'),
        'softImpute' = ferment_stkr_soft,
        'missRanger' = ferment_stkr_rngr,
        'kneighbors' = stop("imputation by stacking isn't ",
          "needed for kneighbor brews. Use test_nbrs() instead.",
          call. = FALSE)
      )

      brew %<>% stkr_fun(
        new_data = timpers[[i]]$data,
        new_name = names(timpers)[i],
        dbl_impute = timpers[[i]]$dbl_impute %||% TRUE
      )

    }

  }

  attr(brew, 'fermented') <- TRUE
  brew

}


#' @rdname ferment
#' @export

is_fermented <- function(brew){
  attr(brew, 'fermented')
}

ferment_nbrs <- function(
  brew,
  new_data,
  new_name,
  dbl_impute,
  neighbor_sequence,
  neighbor_aggregate
){

  verbose        <- get_verbosity(brew)
  verbose_1      <- verbose >= 1
  verbose_2      <- verbose >= 2

  # if new_data is supplied
  outcome <- attr(brew, 'outcome')$name

  # Nothing to do if there are no missing values
  if(!any(is.na(new_data))){

    brew$wort[[new_name]] <- list(new_data)
    attr(brew, 'fermented_cols') %<>% c(new_name)

    if(verbose_1) message(
      glue::glue("No missing values in {new_name} - nothing to impute.")
    )

    return(brew)

  }

  # if new data have missing values
  if(verbose_1)
    message(glue::glue(
      "Fitting models to impute missing values in",
      " {new_name} using nearest neighbors")
    )

  # Move this to outer functions
  check_ferment_data(brew, new_data)

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

  if(dbl_impute){

    imputes <- vector(mode = 'list', length = n_impute)

    for(impute_index in seq_along(imputes)){

      if(verbose_1)
        message(glue::glue(
          "Identifying nearest neighbors ",
          "using dataset {impute_index}")
        )

      nn_index <- gower::gower_topn(
        x = new_data,
        y = brew$wort$training[[impute_index]],
        n = neighbor_sequence[impute_index]
      )$index

      imputes[[impute_index]] <- nn_impute(
        ref_data = brew$wort$training[[impute_index]],
        new_data = new_data,
        nn_index = nn_index,
        neighbors = neighbor_sequence[impute_index],
        random = neighbor_aggregate[impute_index]
      )

    }

    brew$wort[[new_name]] <- imputes

  } else {

    if(verbose_2)
      message(
        glue::glue(
          "Note: Only the original training data ",
          "are being used to impute values in {new_name}"
        )
      )

    brew$wort[[new_name]] <- knn_work(
      ref_data = brew$data,
      new_data = new_data,
      neighbor_sequence = neighbor_sequence,
      neighbor_aggregate = neighbor_aggregate,
      verbose = verbose
    )$fit

  }

  attr(brew, 'fermented_cols') %<>% c(new_name)

  brew

}

stkr_data <- function(brew, new_data, new_name, dbl_impute, verbose){

  if(dbl_impute){

    if(verbose) message(
      glue::glue("Stacking {new_name} data with each imputed training set")
    )

    stacked_data <- purrr::map(
      .x = brew$wort$training,
      .f = ~ dplyr::bind_rows(.x, new_data)
    )

  } else {

    nfit <- brew$pars$n_impute

    stacked_data <- vector(mode = 'list', length = nfit)

    if(verbose >= 1) message(
      glue::glue("Stacking {new_name} data with the unimputed training set")
    )

    for(j in seq(nfit))
      stacked_data[[j]] <- dplyr::bind_rows(brew$data, new_data)

  }

  stacked_data

}

ferment_stkr_soft <- function(
  brew,
  new_data,
  new_name,
  dbl_impute
){

  verbose   <- get_verbosity(brew)
  verbose_1 <- verbose >= 1
  verbose_2 <- verbose >= 2

  nfit <- nrow(brew$wort)
  ntrn <- nrow(brew$data)
  ntst <- nrow(new_data)
  imps <- fits <- vector(mode = 'list', length = nfit)

  stacked_data <- stkr_data(
    brew = brew,
    verbose = verbose_1,
    new_data = new_data,
    new_name = new_name,
    dbl_impute = dbl_impute
  ) %>%
    purrr::map(.f = as.matrix)

  for(j in seq(nfit)){

    args <- list(
      x         = stacked_data[[j]],
      rank.max  = brew$wort$args[[j]]$rank.max,
      lambda    = brew$wort$args[[j]]$lambda,
      type      = brew$wort$args[[j]]$type,
      final.svd = brew$wort$args[[j]]$final.svd
    )

    if(j > 1) args$warm.start <- fits[[j-1]]

    if(brew$pars$scale_data){

      if(verbose_1) message(
        glue::glue("Applying biScale() to stacked data ({j} of {nfit})")
      )

      args$x <- try(
        softImpute::biScale(
          x = args$x,
          maxit = brew$pars$scale_iter,
          trace = verbose_2
        ),
        silent = TRUE
      )

      if(class(args$x)[1] == 'try-error') stop(
        glue::glue("unable to run biScale on {new_name}"),
        call. = FALSE
      )

    }

    fits[[j]] <- do.call(softImpute::softImpute, args = args)

    # Determine the rank of the fit, which may or may not
    # be as high as the maximum rank.
    attr(fits[[j]], 'rank') <- sum(round(fits[[j]]$d, 4) > 0)

    if(verbose_1) print(
      glue::glue(
        "fit {j} of {nfit}: \\
         lambda = {format(round(brew$wort$args[[j]]$lambda, 3),nsmall=3)}, \\
         rank.max = {brew$wort$args[[j]]$rank.max} \\
         rank.fit = {attr(fits[[j]], 'rank')}"
      )
    )


    imps[[j]] <- softImpute::complete(
      x = stacked_data[[j]],
      object = fits[[j]],
      unscale = brew$pars$scale_data
    ) %>%
      tibble::as_tibble() %>%
      dplyr::slice(seq(ntrn+1, ntrn+ntst))

    brew$wort[[new_name]] <- imps

  }

  attr(brew, 'fermented_cols') %<>% c(new_name)

  brew

}

ferment_stkr_rngr <- function(
  brew,
  new_data,
  new_name,
  dbl_impute
) {

  verbose   <- get_verbosity(brew)
  verbose_1 <- verbose >= 1
  verbose_2 <- verbose >= 2

  nfit <- nrow(brew$wort)
  ntrn <- nrow(brew$data)
  ntst <- nrow(new_data)
  imps <- vector(mode = 'list', length = nfit)

  stacked_data <- stkr_data(
    brew = brew,
    verbose = verbose_1,
    new_data = new_data,
    new_name = new_name,
    dbl_impute = dbl_impute
  )

  for(j in seq(nfit)){

    args <- list(
      verbose         = verbose,
      data            = stacked_data[[j]],
      maxiter         = brew$wort$args[[j]]$maxiter,
      num.trees       = brew$wort$args[[j]]$num.trees,
      min.node.size   = brew$wort$args[[j]]$min_node_sizes,
      pmm.k           = brew$wort$args[[j]]$pmm_donor_sizes,
      sample.fraction = brew$wort$args[[j]]$sample.fraction
    )

    imps[[j]] <- do.call(missRanger::missRanger, args = args) %>%
      tibble::as_tibble() %>%
      dplyr::slice(seq(ntrn+1, ntrn+ntst))

  }

  brew$wort[[new_name]] <- imps
  attr(brew, 'fermented_cols') %<>% c(new_name)

  brew

}



