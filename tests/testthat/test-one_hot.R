
test_that(
  "one_hot() works", {

    n <- 10

    DT <- data.table::data.table(
      V1 = seq(n),
      V2 = factor(sample(letters[1:3], n, replace = TRUE)),
      V3 = seq(n) / 10,
      V4 = factor(sample(letters[5:6], n, replace = TRUE))
    )

    DT$V1[1] <- NA
    DT$V3[c(6,7)] <- NA

    DT$V2[1:2] <- NA
    DT$V4[2] <- NA

    dt_1hot <- one_hot(DT)

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


