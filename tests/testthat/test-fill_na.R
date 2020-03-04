
test_that("fill_na works", {

  df <- data.frame(
    a = 1:10,
    b = 21:30,
    y = factor(letters[1:10])
  )

  df_miss <- df

  df_miss$a[c(1, 3, 5, 7)] <- NA
  df_miss$y[3] <- NA

  dt <- data.table::as.data.table(df)
  tb <- tibble::as_tibble(df)

  dt_miss <- data.table::as.data.table(df_miss)
  tb_miss <- tibble::as_tibble(df_miss)

  vals <- list(a = as.integer(c(1, 3, 5, 7)), y = c('c'))

  expect_equal(df, fill_na(df_miss, vals))
  expect_equal(dt, fill_na(dt_miss, vals))
  expect_equal(tb, fill_na(tb_miss, vals))

  bad_vals <- vals
  bad_vals$bad <- c(1)

  expect_error(fill_na(df_miss, bad_vals), 'vals\\$bad')
  expect_error(fill_na(dt_miss, bad_vals), 'vals\\$bad')
  expect_error(fill_na(tb_miss, bad_vals), 'vals\\$bad')

  bad_vals <- vals
  bad_vals$a <- c(bad_vals$a, 1L)

  expect_error(fill_na(df_miss, bad_vals), 'has 5 values')
  expect_error(fill_na(dt_miss, bad_vals), 'has 5 values')
  expect_error(fill_na(tb_miss, bad_vals), 'has 5 values')

  expect_error(fill_na(1, vals), 'unrecognized')

})
