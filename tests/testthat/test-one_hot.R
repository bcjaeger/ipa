
test_that(
  "one_hot() works", {

    n <- 10

    DT <- data.table::data.table(
      V1 = seq(n),
      V2 = factor(sample(letters[1:3], n, replace = TRUE)),
      V3 = seq(n) / 10,
      V4 = sample(letters[5:6], n, replace = TRUE)
    )

    DT$V1[1] <- NA
    DT$V3[c(6,7)] <- NA

    DT$V2[1:2] <- NA
    DT$V4[2] <- NA

    DF <- as.data.frame(DT)
    TB <- tibble::as_tibble(DT)

    dt_1hot <- one_hot(DT)
    df_1hot <- one_hot(DF)
    tb_1hot <- one_hot(TB)

    expect_is(dt_1hot, 'data.table')
    expect_is(df_1hot, 'data.frame')
    expect_is(tb_1hot, 'tbl_df')

    expect_equal(df_1hot, as.data.frame(dt_1hot))
    expect_equal(tibble::as_tibble(df_1hot), tb_1hot)

    expect_equal(
      colnames(dt_1hot),
      c("V1", "V2_a", "V2_b", "V2_c", "V3", "V4_e", "V4_f")
    )

    # check that missing values for V2 are correct
    v2_miss_correct <- all(
      is.na( dt_1hot[1:2, grep("V2", colnames(dt_1hot)), with = FALSE] )
    )

    expect_true(v2_miss_correct)

  }
)

test_that(
  'insert_vals() works',
  {

    vec <- c(1)

    expect_error(insert_vals(vec, where = 1, what = 'a'))

    expect_equal(insert_vals(vec, where = 1, what = 2), 2)

    vec <- c(1,2)

    expect_equal(insert_vals(vec, where = 1, what = c(-2, -1, 0, 1)), -2:2)

    expect_equal(insert_vals(vec, where = 2, what = c(2,3)), 1:3)

    vec <- c(1,2,3)

    expect_equal(insert_vals(vec, where = 2, what = -2), c(1,-2,3))

    expect_equal(
      insert_vals(vec, where = 2, what = c(-1, -1)), c(1, -1, -1, 3)
    )


  }
)

test_that(
  'one_hot_vec() works', {

    x <- c(0, 1, 0, 1)

    expect_equal(
      one_hot_vec(x, ncats = 2),
      structure(c(1, 0, 1, 0, 0, 1, 0, 1), .Dim = c(4L, 2L))
    )

  }
)

test_that(
  'one_hot_chr() works', {

    x <- c(letters[c(1,2,1)])
    lvls <- c('a', 'b')

    expect_equal(
      one_hot_chr(x, lvls),
      structure(c(1, 0, 1, 0, 1, 0), .Dim = 3:2)
    )

  }
)

