
test_that("fill_na works", {

  df <- data.frame(
    a = 1:10,
    b = 21:30,
    y = factor(letters[1:10])
  )

  df_miss <- df

  df_miss$a[c(1, 3, 5, 7)] <- NA
  df_miss$y[3] <- NA

  vals <- list(a = c(1, 3, 5, 7), y = c('c'))

  expect_equal(df, fill_na(df_miss, vals))

  vals <- list(a = c(1,2,4), y = c('c'))

  expect_error(fill_na(df_miss, vals), 'missing values')

})
