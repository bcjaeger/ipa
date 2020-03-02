
test_that("mindx works", {

  df <- data.table::data.table(
    a = rnorm(10),
    b = rbinom(10, size = 1, prob = 1/2),
    y = factor(sample(letters[1:5], size = 10, replace = T))
  )

  # nothing should happen b/c no missing values
  df_bind <- mindx(df, attach = TRUE, drop_const = TRUE)
  expect_equal(df_bind, df)

  df_miss <- df

  df_miss$a[sample(5)] <- NA
  df_miss$y[sample(5)] <- NA

  df_miss_bind <- mindx(df_miss, sep = '.', miss_chr = 'na',
    drop_const = FALSE, attach = TRUE)

  expect_equal(df_miss_bind$a, df_miss$a)

  expect_equal(df_miss_bind$b, df_miss$b)

  expect_equal(df_miss_bind$y, df_miss$y)

  expect_equal(df_miss_bind$a.na, as.integer(is.na(df_miss$a)))

  expect_equal(df_miss_bind$b.na, as.integer(is.na(df_miss$b)))

  expect_equal(df_miss_bind$y.na, as.integer(is.na(df_miss$y)))

  df_miss_alone <- mindx(df_miss,
    sep = '.', miss_chr = 'na',
    drop_const = FALSE,
    attach = FALSE
  )

  expect_equal(df_miss_alone, df_miss_bind[, -c(1:3)])

})
