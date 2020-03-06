

test_that("imputations works",
  {

    n <- 10

    DT_orig <- DT <- data.table::data.table(
      V1 = seq(n),
      V2 = factor(sample(letters[1:3], n, replace = TRUE)),
      V3 = seq(n) / 10,
      V4 = sample(letters[5:6], n, replace = TRUE)
    )

    DT$V1[1] <- NA
    DT$V2[1:2] <- NA
    DT$V3[c(6,7)] <- NA
    DT$V4[2] <- NA

    DT_new <- data.table::data.table(
      V1 = seq(n),
      V2 = factor(sample(letters[1:3], n, replace = TRUE)),
      V3 = seq(n) / 10,
      V4 = sample(letters[5:6], n, replace = TRUE)
    )

    DT_new$V1[2] <- NA
    DT_new$V2[3:5] <- NA
    DT_new$V3[c(1,6)] <- NA
    DT_new$V4[9] <- NA

    expect_warning(impute_soft(DT_orig), 'no missing values')

    TB <- tibble::as_tibble(DT)
    TB_new <- tibble::as_tibble(DT_new)

    expect_error(impute_soft(TB, cols = V1), '2\\+ are needed')
    expect_error(impute_soft(DT, cols = V1), '2\\+ are needed')

    bad_data <- TB
    bad_data$const_col = 1

    expect_error(impute_soft(bad_data), 'column scale')

    bad_data <- dplyr::select(TB, V1, V3)
    expect_error(impute_soft(bad_data, bs_row.scale = T), 'row scale')

    dt_imps_trn <- impute_soft(DT, cols = -V1)

    expect_true(
      all(names(dt_imps_trn$imputed_values[[1]]) %in% c("V3", "V2", "V4"))
    )


    set.seed(1)
    dt_imps_tst <- impute_soft(DT, data_new = TB_new)

    set.seed(1)
    tb_imps_tst <- impute_soft(TB, data_new = DT_new)

    expect_equal(dt_imps_tst$imputed_values, tb_imps_tst$imputed_values)

    tb_imps <- impute_soft(TB, restore_data = FALSE)

    tb_filled <- fill_na(data = one_hot(DT),
      vals = tb_imps$imputed_values[[1]])

    expect_true(all(sapply(tb_filled, typeof)=='double'))

    dt_imps_1hot <- impute_soft(DT, restore_data = FALSE, grid = TRUE)

    expect_true(nrow(dt_imps_1hot) == 20)

    expect_true(
      all(sapply(dt_imps_1hot$imputed_values[[1]], get_var_type) == 'ctns')
    )

  }
)
