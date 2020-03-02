
test_that(
  "imputation works",
  {

    set.seed(329)
    df <- tibble::tibble(
      a = rnorm(100),
      b = a*runif(100) / 10,
      c = a - 1 + runif(100),
      y = 1 + a - 2*b + c/2 + rnorm(100)
    )

    df$b[c(1, 10, 50, 100)] <- NA

    trn <- df[51:100, ]
    tst <- df[1:50, ]

    # imputing training data

    knn_imps <- impute_nbrs(
      data_ref = trn,
      k_neighbors = 1:5,
      cols = -y,
      fun_aggr_intg = mean
    )

    # make sure imputes are the same as recipes
    # reci_trn <- recipes::recipe(trn, y ~ .) %>%
    #   recipes::step_knnimpute(recipes::all_predictors()) %>%
    #   recipes::prep()
    #
    # reci_trn_answer <- recipes::juice(reci_trn) %>%
    #   dplyr::select(b) %>%
    #   dplyr::slice(50) %>%
    #   purrr::pluck('b')

    expect_equal(knn_imps$imputed_values[[5]]$b[1], 0.01686512)

    # on testing data
    knn_imps <- impute_nbrs(
      data_ref = trn,
      data_new = tst,
      k_neighbors = c(1,3,5),
      cols = -c(y),
      fun_aggr_intg = mean
    )

    # reci_tst <- recipes::bake(reci_trn, new_data = tst)
    #
    # reci_tst_answer <- reci_tst %>%
    #   dplyr::select(b) %>%
    #   dplyr::slice(1) %>%
    #   purrr::pluck('b')

    expect_equal(knn_imps$imputed_values[[3]]$b[1], 0.01243823)

    trn$fctr <- factor(rbinom(50, 1, prob = abs(trn$a) / max(abs(trn$a))),
      labels = c("No", "Yes"))
    tst$fctr <- factor(rbinom(50, 2, prob = abs(tst$a) / max(abs(tst$a))),
      labels = c("No", "Yes", "Maybe"))

    expect_error(
      impute_nbrs(data_ref = tst, cols = -c, k_neighbors = c(1, 10, 20, 50)),
      regexp = 'exceed or match'
    )

  }
)
