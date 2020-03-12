
n <- 10

DT_orig <- DT <- data.table::data.table(
  V1 = seq(n),
  V2 = factor(letters[c(3,3,3,2,2,2,1,1,1,1)]),
  V3 = seq(n) / 10,
  V4 = letters[rep(c(1,2), each = 5)]
)

DT$V1[1] <- NA
DT$V2[1:2] <- NA
DT$V3[c(6,7)] <- NA
DT$V4[2] <- NA

DT_new <- data.table::data.table(
  V1 = seq(n),
  V2 = factor(letters[c(3,3,3,3,2,2,1,2,1,1)]),
  V3 = seq(n) / 10,
  V4 = letters[rep(c(1,3), each = 5)]
)

DT_new$V1[2] <- NA
DT_new$V2[3:5] <- NA
DT_new$V3[c(1,6)] <- NA
DT_new$V4[9] <- NA

expect_error(impute_nbrs(DT, k_neighbors = 1:9), '\\(8 and 9\\)')

imps_ref <- impute_nbrs(DT, k_neighbors = c(1:5))

imps_new <- impute_nbrs(DT, data_new = DT_new, k_neighbors = 1:5)

imps_ref_rando <- impute_nbrs(DT, k_neighbors = c(1:5), aggregate = FALSE)

imputed_types <- purrr::map_chr(
  .x = imps_ref$imputed_values[[1]],
  .f = ~class(.x)[1]
)

test_that('imputation maintains variable types', {
  expect_equal(
    imputed_types,
    c(V1 = "integer", V2 = "factor", V3 = "numeric", V4 = "character")
  )
})

test_that(
  "impute_nbrs matches recipes",
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
