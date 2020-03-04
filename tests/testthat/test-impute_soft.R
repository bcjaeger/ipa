

test_that("imputations works",
  {

    n <- 10

    DT_orig <- DT <- data.table::data.table(
      V1 = seq(n),
      V2 = factor(sample(letters[1:3], n, replace = TRUE)),
      V3 = seq(n) / 10,
      V4 = factor(sample(letters[5:6], n, replace = TRUE))
    )

    DT$V1[1] <- NA
    DT$V2[1:2] <- NA
    DT$V3[c(6,7)] <- NA
    DT$V4[2] <- NA

    dt_imps <- impute_soft(DT)

    truth <- list(
      V1 = DT_orig$V1[1],
      V2 = DT_orig$V2[1:2],
      V3 = DT_orig$V3[6:7],
      V4 = DT_orig$V4[2]
    )

    expect_equal(
      sapply(dt_imps$imputed_values[[10]], ipa:::get_var_type)[names(DT)],
      sapply(DT, get_var_type)[names(DT)]
    )

    dt_imps_1hot <- impute_soft(DT, restore_data = FALSE)

    expect_true(
      all(sapply(dt_imps_1hot$imputed_values[[1]], get_var_type) == 'ctns')
    )

  }
)
