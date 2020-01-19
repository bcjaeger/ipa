
#' a simple framework to simulate simple dataframes
#'
#'  Simulated data allow analysts to conduct controlled experiments
#'  using specific parameters to generate data.
#'
#'  All simulated predictor variables are numeric. Regression coefficients
#'  are generated randomly (all values are between -1 and 1).
#'
#' @param problem_type A character value indicating the problem type to
#'   simulate data for. Valid options are 'regression', 'classification',
#'   or 'survival'.
#' @param ncov the number of main effects used to generate an outcome variable
#' @param nint the number of interaction effects used to generate an outcome
#'   variable.
#' @param ngrp the number of X groups in the data. Each X group will have
#'   different mean values for the `ncov` predictor variables, but will
#'   not have different rules governing the relationship between response
#'   and predictor variables.
#' @param degree the degree of each predictor variable's relationship to
#'   the outcome. For example, `degree = 2` makes the relationship between
#'   each predictor variable and the outcome quadratic.
#' @param rho the correlation coefficient among predictors in the X matrix.
#' @param corstr The correlation structure among predictors in the X matrix.
#' @param nobs the total number of observations in the simulated data.
#' @param error_sd the standard deviation of error applied when generating
#'   outcome values.
#' @param prevalence the prevalence of the outcome.
#'   (only relevant for classification problems).
#' @param split_prop the proportion of data that will be randomly assigned
#'   to the training dataset.
#'
#' @export
#'
#' @examples
#'
#' regr = gen_simdata(problem_type = 'regression',
#'   ncov = 3, nint = 2, degree = 3, nobs = 2000)
#'
#' clsf = gen_simdata(problem_type = 'classification',
#'   ncov = 3, nint = 2, degree = 3, nobs = 2000)
#'
#'
#' surv = gen_simdata(problem_type = 'survival',
#'   ncov = 3, nint = 2, degree = 3, nobs = 2000)

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
# miss_mechanism = 'mar'


gen_simdata <- function(
  problem_type = c('regression', 'classification', 'survival'),
  ncov = 3,
  nint = 2,
  ngrp = 1,
  degree = 3,
  rho = 1/2,
  corstr = c('AR1','CS'),
  nobs = 10000,
  error_sd = 1/2,
  prevalence = NULL,
  split_prop = 1/2
){

  problem_type = problem_type[1]
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

  if(ngrp == 1) x_mean = 0

  if(ngrp > 1) x_mean = seq(-1, 1, length.out = ngrp)

  x_obsr <- purrr::map(
    .x = x_mean,
    .f = ~ mvtnorm::rmvnorm(
      mean = rep(.x, ncov),
      n = round(nobs / ngrp),
      sigma = Sigma
    ) %>%
      magrittr::set_colnames(xnames)
  ) %>%
    purrr::reduce(rbind)

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
      group = rep(1:ngrp, each = round(nobs / ngrp)),
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
      group = rep(1:ngrp, each = round(nobs / ngrp)),
      tibble::as_tibble(x_obsr)
    )

  }

  trn_indx <- sample(
    x = 1:nrow(data),
    size = round(split_prop * nrow(data))
  )

  list(
    beta = beta,
    train = tibble::as_tibble(data[trn_indx, ]),
    test = tibble::as_tibble(data[-trn_indx, ])
  )


}

#' Ampute data
#'
#' This function provides a general framework to simulate missing data
#'   under different mechanisms:
#'
#'  1. missing completely at random (MCAR): missingness occurs at random,
#'     independent of all variables
#'
#'  2. missing at random (MAR): missingness occurs at random, conditional
#'     on measured variables in the observed data.
#'
#'  3. missing not at random (MNAR) missingess occurs non-randomly and is
#'     dependent on unmeasured variables.
#'
#' Notably, this function is a wrapper for the more granular [mice::ampute()]
#' function. The current function creates missing patterns and types at
#' random, whereas [mice::ampute()] allows them to be specified manually.
#'
#' @param data data to ampute
#'
#' @param miss_proportion the proportion of `data` that will
#'   be set to missing.
#'
#' @param by_cases logical. If `TRUE`, the proportion of missingness is
#'   defined in terms of cases. If `FALSE`, the proportion of missingness
#'   is defined in terms of cells. Default is `TRUE`. Setting to `FALSE`
#'   may cause errors when the desired proportion of missingness cannot
#'   be obtained.
#'
#' @param omit_cols column names of variables that will not be amputed.
#'
#' @param miss_cols_range an integer vector of length 2 containing
#'   The minimum followed by maximum number of columns that
#'   can be set to missing in any given missing pattern. If nothing is
#'   specified, then the minimum number of columns will be set to 1 and
#'   the maximum number of columns will be set to  the number of
#'   columns in `data` minus the number of columns in `omit_cols`.
#'
#' @param miss_mech a string specifying the missingness mechanism,
#'   either MCAR (Missing Completely At Random), MAR (Missing At Random)
#'   or MNAR (Missing Not At Random). Default is a MCAR mechanism.
#'
#' @param miss_type a vector of strings containing the type of missingness
#' for each pattern. Either "LEFT", "MID", "TAIL" or '"RIGHT". If a single
#'  missingness type is entered, all patterns will be created by the same
#'  type. If missingness types should differ over patterns, a vector of
#'  missingness types should be entered. If nothing is specified,
#'  missing patterns will be generated at random.
#'
#' @param miss_ptrn_count integer. The desired number of missing patterns
#'   for the amputed data. Patterns are generated at random. Default is 10.
#'
#' @param miss_ptrn_prop a vector of length equal `miss_ptrn_count`
#'   containing the relative frequency with which the patterns should occur.
#'   For example, for three missing data patterns, the vector could be
#'   `c(0.4, 0.4, 0.2)`, meaning that of all cases with missing values,
#'   40% should have pattern 1, 40% should have pattern 2, and 20% should
#'   have pattern 3. The vector should sum to 1. If nothing is specified,
#'   then `miss_ptrn_prop` is created using random values.
#'
#' @export
#'

add_missing <- function(
  data,
  miss_proportion,
  by_cases = TRUE,
  omit_cols = NULL,
  miss_cols_range = NULL,
  miss_mech = c('mcar','mar','mnar'),
  miss_type = NULL,
  miss_ptrn_count = 10,
  miss_ptrn_prop = NULL
){

  miss_mech <- toupper(miss_mech[1])

  fctr_data <- data %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::select_if(is.factor) %>%
    purrr::map(levels) %>%
    tibble::enframe(name = 'variable', value = 'levels') %>%
    dplyr::mutate(
      col_names = purrr::map2(
        .x = variable,
        .y = levels,
        .f = paste,
        sep = '_'
      )
    )

  if(nrow(fctr_data) > 0){

    names_orig <- names(data)

    fctr_levels <- fctr_data %>%
      dplyr::select(variable, levels) %>%
      tibble::deframe()

    data <- herdCats::cat_spread(data)

    names_spread <- names(data)

    names_okay <- purrr::map_lgl(
      .x = fctr_data$col_names,
      .f = ~ all(.x %in% names_spread)
    )
    if(!all(names_okay)){
      stop("factor names are not formatted correctly")
    }
  } else {

    names_spread <- names_orig <- names(data)

  }

  n_col = ncol(data)

  ptrn <- matrix(1, ncol = n_col, nrow = miss_ptrn_count)
  colnames(ptrn) <- names_spread

  miss_names <- setdiff(names_orig, omit_cols)

  if(!is.null(miss_cols_range)){

    if(min(miss_cols_range) >= length(miss_names)){
      stop("Minimum of miss_cols_range should be < no. of columns",
        " that can be set to missing", call. = FALSE)
    }

    cols_missing <- seq(min(miss_cols_range), max(miss_cols_range))

  } else {
    cols_missing <- seq(length(miss_names))
  }

  for(i in seq(miss_ptrn_count)){

    set_zero_count <- sample(x = cols_missing, 1)
    set_zero_count <- min(set_zero_count, length(miss_names))
    set_zero_names <- sample(x = miss_names, set_zero_count)

    set_zero_colnames <- c()

    for(j in set_zero_names){

      if(j %in% fctr_data$variable){
        index <- which(fctr_data$variable == j)
        set_zero_colnames %<>% c(fctr_data$col_names[[index]])
      } else {
        set_zero_colnames %<>% c(j)
      }

    }

    ptrn[i, set_zero_colnames ] <- 0

  }

  # remove duplicate rows

  any_dup_rows <- nrow(unique(ptrn)) < miss_ptrn_count

  if(any_dup_rows){

    warning("some missing patterns were duplicates", call. = FALSE)

    ptrn  <- unique(ptrn)
    miss_ptrn_count <- nrow(ptrn)

  }

  # create freq, a vector with relative frequency of each pattern
  freq <- miss_ptrn_prop %||% stats::runif(n = miss_ptrn_count)

  if(length(freq) == 1) freq <- rep(freq, miss_ptrn_count)

  if(length(freq) != miss_ptrn_count){
    warning('freq is not the same length as #patterns. ',
      'Random values will be used.',call. = FALSE)
    freq <- stats::runif(n = miss_ptrn_count)
  }


  # freq's values should sum to 1
  freq <- freq / sum(freq)

  type <- miss_type %||% sample(
    x = c("LEFT","RIGHT","MID","TAIL"),
    size = nrow(ptrn),
    replace = TRUE
  )

  output <- data %>%
    mice::ampute(
      prop = miss_proportion,
      bycases = by_cases,
      patterns = ptrn,
      freq = freq,
      type = type,
      mech = miss_mech
    ) %>%
    magrittr::use_series('amp') %>%
    tibble::as_tibble()

  if(nrow(fctr_data) > 0){
    herdCats::cat_gather(output, factor_levels = fctr_levels)
  } else {
    output
  }



}



non_lin <- function(xvals, xname, degree){

  seq(degree) %>%
    purrr::map(function(dgr) xvals^dgr) %>%
    purrr::set_names(glue::glue("{xname}_raiseto_{seq(degree)}")) %>%
    dplyr::bind_cols()

}
