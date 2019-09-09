
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


# problem_type = 'survival'
# ncov = 50
# nint = 20
# rho = 1/2
# nobs = 10000
# error_sd = 1/2
# prevalence = NULL
# split_prop = 1/2
# miss_pattern = 'mar'
# trn_miss_prop = 1/2
# tst_miss_prop = 0

gen_simdata <- function(
  problem_type = 'regression',
  ncov = 50,
  nint = 20,
  rho = 1/2,
  nobs = 10000,
  error_sd = 1/2,
  prevalence = NULL,
  split_prop = 1/2,
  miss_pattern = 'mar',
  trn_miss_prop = 1/2,
  tst_miss_prop = 0
){

  # Main effects
  xnames = paste0('x', 1:ncov)

  beta <- runif(
    n = ncov,
    min = -1,
    max = 1
  ) %>%
    divide_by(sqrt(ncov)) %>%
    set_names(xnames)

  # Interaction effects
  if(nint > 0){

    int_ind <- replicate(
      n = nint,
      expr = sample(xnames, 2),
      simplify = FALSE
    )

    inames = map_chr(
      .x = int_ind,
      .f = ~glue("{.x[1]}_i_{.x[2]}")
    )

    icoefs <- runif(n = nint, min = -1, max = 1) %>%
      divide_by(sqrt(ncov)) %>%
      set_names(inames)

    beta %<>% c(icoefs)

  }

  # Covariance matrix between fixed effects
  Sigma = matrix(0, ncol = ncov, nrow = ncov)

  # Impose an autoregressive covariance structure
  # between the fixed effects. Since each x variable
  # has unit variance, cov(X) = corr(X)

  for (i in 1:ncov) {
    for (j in 1:ncov) {

      Sigma[i, j] = rho ^ (abs(i - j))

    }
  }

  diag(Sigma) <- 1

  x_true = x_obsr <- mvtnorm::rmvnorm(
    mean = rep(0, nrow(Sigma)),
    n = nobs,
    sigma = Sigma
  ) %>%
    set_colnames(xnames)

  if(nint > 0){

    intr = map(
      .x = int_ind,
      .f =  function(i) x_obsr[, i[1]] * x_obsr[, i[2]]
    ) %>%
      set_names(names(icoefs)) %>%
      bind_cols()

    x_true %<>% cbind(intr)

  }

  if(problem_type == 'survival'){

    y <- simsurv(
      dist = 'weibull',
      lambdas = 0.1,
      gammas = 1.5,
      betas = beta,
      x = x_true,
      maxt = 10
    )

    data <- bind_cols(
      time = y$eventtime,
      status = y$status,
      as_tibble(x_obsr)
    )

  } else {

    y <- as.matrix(x_true) %*% matrix(beta) + rnorm(n = nobs, sd = error_sd)
    y <- as.numeric(y)

    if(problem_type=='classification'){

      cut_prop <- if(!is.null(prevalence)) prevalence else 0.50

      y <- ifelse(y > quantile(y, probs = 1-cut_prop), 1, 0)
      y <- factor(y, levels = c(0,1), labels = c("No","Yes"))

    }

    data <- bind_cols(response = y, as_tibble(x_obsr))

  }

  trn_indx <- sample(
    x = 1:nrow(data),
    size = round(split_prop * nrow(data))
  )

  orig <- list(
    trn = as_tibble(data[trn_indx, ]),
    tst = as_tibble(data[-trn_indx, ])
  )

  patterns <-
    ampute.default.patterns(n = ncol(orig$trn)) %>%
    set_colnames(names(orig$trn)) %>%
    .[1:min(10, ncov), ]

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
    divide_by( sum(1:nrow(patterns)) )

  type <- sample(
    x = c("LEFT","RIGHT","MID","TAIL"),
    size = nrow(patterns),
    replace = TRUE)

  fctrs <- map_chr(orig$trn, class) %>%
    enframe() %>%
    filter(value == 'factor') %>%
    mutate(value = map(name, ~levels(orig$trn[[.x]]))) %>%
    deframe()

  output <- map2(
    .x = orig,
    .y = list(trn_miss_prop, tst_miss_prop),
    .f = function(df, miss_prop){
      if(miss_prop == 0){ return(as_tibble(df)) }
      miss_df <- df %>%
        ampute(
          prop = miss_prop,
          patterns = patterns,
          freq = freq,
          type = type,
          mech = toupper(miss_pattern)
        ) %>%
        use_series('amp') %>%
        as_tibble()
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
#' @export
add_missing <- function(
  data,
  omit_cols,
  miss_proportion,
  miss_pattern
){

  ncov = ncol(data)

  patterns <-
    ampute.default.patterns(n = ncol(data)) %>%
    set_colnames(names(data)) %>%
    .[1:min(10, ncov), ]

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
    divide_by( sum(1:nrow(patterns)) )

  type <- sample(
    x = c("LEFT","RIGHT","MID","TAIL"),
    size = nrow(patterns),
    replace = TRUE
  )

  fctrs <- map_chr(data, class) %>%
    enframe() %>%
    filter(value == 'factor') %>%
    mutate(value = map(name, ~levels(data[[.x]]))) %>%
    deframe()

  miss_df <- suppressWarnings(
    ampute(
      data = data,
      prop = miss_proportion,
      patterns = patterns,
      freq = freq,
      type = type,
      mech = toupper(miss_pattern)
    ) %>%
      use_series('amp') %>%
      as_tibble()
  )

  for(f in names(fctrs)){
    miss_df[[f]] %<>%
      factor(
        levels = 1:length(fctrs[[f]]),
        labels = fctrs[[f]]
      )
  }

  miss_df

}
