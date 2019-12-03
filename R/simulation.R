
#' a simple framework to simulate simple dataframes
#'
#' @param problem_type A character value indicating the problem type to
#'   simulate data for. Valid options are 'regression', 'classification',
#'   or 'survival'.
#' @param ncov the number of main effects used to generate an outcome variable
#' @param nint the number of interaction effects used to generate an outcome
#'   variable.
#' @param rho the auto correlation constant among predictors in the X matrix.
#' @param nobs the total number of observations in the simulated data.
#' @param error_sd the standard deviation of error applied when generating
#'   outcome values.
#' @param prevalence (this is only relevant for classification problems).
#'   The prevalence of the outcome.
#' @param split_prop the proportion of data that will be randomly assigned
#'   to the training dataset.
#' @param miss_pattern A character value indicating what type of missingness
#'   pattern to apply. Valid options are 'mcar', 'mar', and 'mnar'.
#'   mcar = missing completely at random, mar = missing at random, and
#'   mnar = missing not at random.
#' @param trn_miss_prop The proportion of data in the training set that will
#'   be set to missing.
#' @param tst_miss_prop The proportion of data in the testing set that will
#'   be set to missing.
#' @export
#'
#' @examples
#'

#' gen_simdata(ncov = 1, nint = 0, nobs = 100)
#' gen_simdata(ncov = 2, nint = 2, nobs = 100)


# problem_type = 'regression'
# ncov = 5
# nint = 4
# degree = 3
# rho = 1/2
# corstr = c('AR1','CS')
# nobs = 10000
# error_sd = 1/2
# prevalence = NULL
# split_prop = 1/2
# miss_pattern = 'mar'
# trn_miss_prop = 1/2
# tst_miss_prop = 1/2


gen_simdata <- function(
  problem_type = 'regression',
  ncov = 3,
  nint = 2,
  degree = 3,
  rho = 1/2,
  corstr = c('AR1','CS'),
  nobs = 10000,
  error_sd = 1/2,
  prevalence = NULL,
  split_prop = 1/2,
  miss_pattern = 'mar',
  npatterns = 10,
  trn_miss_prop = 1/2,
  tst_miss_prop = 0
){

  corstr = corstr[1]

  # Main effects
  xnames = paste0('x', 1:ncov)

  # Covariance matrix between fixed effects
  Sigma = matrix(0, ncol = ncov, nrow = ncov)

  # Impose an autoregressive covariance structure
  # between the fixed effects. Since each x variable
  # has unit variance, cov(X) = corr(X)

  for (i in 1:ncov) {
    for (j in 1:ncov) {

      expo <- switch(
        corstr,
        'AR1' = abs(i-j),
        'CS' = 1L
      )

      Sigma[i, j] = rho^expo

    }
  }

  diag(Sigma) <- 1

  x_obsr <- mvtnorm::rmvnorm(
    mean = rep(0, nrow(Sigma)),
    n = nobs,
    sigma = Sigma
  ) %>%
    magrittr::set_colnames(xnames)

  x_true <- purrr::map2(
    .x = tibble::as_tibble(x_obsr),
    .y = xnames,
    .f = non_lin,
    degree = degree
  ) %>%
    dplyr::bind_cols()

  beta <- stats::runif(
    n = ncol(x_true),
    min = -1,
    max = 1
  ) %>%
    magrittr::divide_by(sqrt(ncol(x_true))) %>%
    purrr::set_names(names(x_true))

  # Interaction effects
  if(nint > 0){

    interactions <- outer(X = xnames, Y = xnames, FUN = paste, sep = '_i_')
    interactions <- interactions[upper.tri(interactions)]

    max_intr <- length(interactions)

    if(nint > max_intr) stop(
      glue::glue("maximum no. of interactions for",
        "ncov = {ncov} is {max_intr}"),
      call. = FALSE
    )

    inames <- sample(interactions, nint)

    icoefs <- stats::runif(n = nint, min = -1, max = 1) %>%
      magrittr::divide_by(sqrt(ncov)) %>%
      magrittr::set_names(inames)

    beta %<>% c(icoefs)

    intr <- names(icoefs) %>%
      purrr::set_names() %>%
      strsplit("_i_", fixed = TRUE) %>%
      purrr::map(~x_obsr[,.x[1]] * x_obsr[,.x[2]]) %>%
      dplyr::bind_cols()

    x_true %<>% cbind(intr)


  }

  if(problem_type == 'survival'){

    y <- simsurv::simsurv(
      dist = 'weibull',
      lambdas = 0.1,
      gammas = 1.5,
      betas = beta,
      x = as.data.frame(x_true),
      maxt = 10
    )

    data <- dplyr::bind_cols(
      time = y$eventtime,
      status = y$status,
      tibble::as_tibble(x_obsr)
    )

  } else {

    error_term <- stats::rnorm(n = nobs, sd = error_sd)

    y <- as.matrix(x_true) %*% matrix(beta) + error_term
    y <- as.numeric(y)

    if(problem_type=='classification'){

      cut_prop <- if(!is.null(prevalence)) prevalence else 0.50

      y <- ifelse(y > stats::quantile(y, probs = 1-cut_prop), 1, 0)
      #y <- factor(y, levels = c(0,1), labels = c("No","Yes"))

    }

    data <- dplyr::bind_cols(
      response = y,
      tibble::as_tibble(x_obsr)
    )

  }

  trn_indx <- sample(
    x = 1:nrow(data),
    size = round(split_prop * nrow(data))
  )

  orig <- list(
    trn = tibble::as_tibble(data[trn_indx, ]),
    tst = tibble::as_tibble(data[-trn_indx, ])
  )


  patterns <-
    mice::ampute.default.patterns(n = ncol(orig$trn)) %>%
    magrittr::set_colnames(names(orig$trn)) %>%
    .[1:min(npatterns, ncov), , drop = FALSE]

  for(i in 1:nrow(patterns)){
    patterns[i, 1:ncol(patterns)] <- sample(
      x = c(1,0),
      size = ncol(patterns),
      replace = TRUE
    )
  }

  if(problem_type == 'survival'){
    patterns[, c('time', 'status')] <- 1L
  } else {
    patterns[,'response'] <- 1L
  }

  patterns %<>% unique()

  freq <- sample(x = nrow(patterns)) %>%
    magrittr::divide_by( sum(1:nrow(patterns)) )

  type <- sample(
    x = c("LEFT","RIGHT","MID","TAIL"),
    size = nrow(patterns),
    replace = TRUE
  )

  fctrs <- purrr::map_chr(orig$trn, class) %>%
    tibble::enframe() %>%
    dplyr::filter(value == 'factor') %>%
    dplyr::mutate(
      value = purrr::map(
        .x = name,
        .f = ~levels(orig$trn[[.x]])
      )
    ) %>%
    tibble::deframe()

  output <- purrr::map2(
    .x = orig,
    .y = list(trn_miss_prop, tst_miss_prop),
    .f = function(df, miss_prop){
      if(miss_prop == 0){ return(tibble::as_tibble(df)) }
      miss_df <- df %>%
        mice::ampute(
          prop = miss_prop,
          patterns = patterns,
          freq = freq,
          type = type,
          mech = toupper(miss_pattern)
        ) %>%
        magrittr::use_series('amp') %>%
        tibble::as_tibble()
      for(f in names(fctrs)){
        miss_df[[f]] %<>%
          factor(
            levels = 1:length(fctrs[[f]]),
            labels = fctrs[[f]]
          )
      }
      miss_df
    }
  )

  list(
    beta = beta,
    training = output$trn,
    testing = output$tst
  )

}

#' easy wrapper for ampute
#'
#' @param data data to ampute
#' @param omit_cols column names of variables that will not be amputed
#' @param miss_proportion proportion of data that will be amputed
#' @param miss_pattern the pattern of missing data. valid options
#'  are 'mar','mcar', and 'mnar'.
#'
#' @export
add_missing <- function(
  data,
  omit_cols,
  miss_proportion,
  miss_pattern,
  npatterns = 10
){

  ncov = ncol(data)

  patterns <-
    mice::ampute.default.patterns(n = ncol(data)) %>%
    magrittr::set_colnames(names(data)) %>%
    .[1:min(npatterns, ncov), , drop = FALSE]

  for(i in 1:nrow(patterns)){

    patterns[i, 1:ncol(patterns)] <- sample(
      x = c(1,0),
      size = ncol(patterns),
      replace = TRUE
    )

  }

  patterns[, omit_cols] <- 1L

  patterns %<>% unique()

  freq <- sample(x = nrow(patterns)) %>%
    magrittr::divide_by( sum(1:nrow(patterns)) )

  type <- sample(
    x = c("LEFT","RIGHT","MID","TAIL"),
    size = nrow(patterns),
    replace = TRUE
  )

  fctrs <- purrr::map_chr(data, class) %>%
    tibble::enframe() %>%
    dplyr::filter(value == 'factor') %>%
    dplyr::mutate(
      value = purrr::map(
        .x = name,
        .f = ~levels(data[[.x]])
      )
    ) %>%
    tibble::deframe()

  miss_df <- suppressWarnings(
    mice::ampute(
      data = data,
      prop = miss_proportion,
      patterns = patterns,
      freq = freq,
      type = type,
      mech = toupper(miss_pattern)
    ) %>%
      magrittr::use_series('amp') %>%
      tibble::as_tibble()
  )

  for(f in names(fctrs)){
    if(is.numeric(miss_df[[f]])){
      miss_df[[f]] %<>%
        factor(
          levels = 1:length(fctrs[[f]]),
          labels = fctrs[[f]]
        )
    }
  }

  miss_df

}


non_lin <- function(xvals, xname, degree){

  seq(degree) %>%
    purrr::map(function(dgr) xvals^dgr) %>%
    purrr::set_names(glue::glue("{xname}_raiseto_{seq(degree)}")) %>%
    dplyr::bind_cols()

}
