
df1 <- data.frame(
  x1 = c(NA, 1),
  x2 = c(2, NA)
)

df2 <- df1
df2$x3 <- c(3,3)

test_that("mindx works", {

  expect_equal(mindx(df1, drop_empty = TRUE), list(x1=1, x2=2))
  expect_equal(mindx(df1, drop_empty = FALSE), list(x1=1, x2=2))

  expect_equal(mindx(df2, drop_empty = TRUE), list(x1=1, x2=2))
  expect_equal(mindx(df2, drop_empty = FALSE), list(x1=1, x2=2, x3=integer()))

})


null_indx <- NULL
empty_indx <- integer()
real_indx <- c(1,2)
N=3

test_that("as_indicator works", {

  expect_equal(as_indicator(null_indx, N), c(0,0,0))
  expect_equal(as_indicator(empty_indx, N), c(0,0,0))
  expect_equal(as_indicator(real_indx, N), c(1,1,0))

})

dt_ref <- as.data.table(df2)
dt_new <- data.table(x1 = 1, x2 = 2, x3 = NA_real_)

test_that('.bind_miss works', {

  expect_equal(
    as.data.frame(.bind_miss(copy(dt_ref))),
    data.frame(
      x1 = c(NA, 1),
        x2 = c(2, NA),
        x3 = c(3, 3),
        x1_missing = 1:0,
        x2_missing = 0:1
    )
  )

  expect_equal(
    as.data.frame(.bind_miss(copy(dt_new))),
    data.frame(x1 = 1, x2 = 2, x3 = NA_real_, x3_missing = 1L)
  )

  .ix  <- mindx(dt_ref)
  .ref <- .bind_miss(copy(dt_ref))
  .new <- .bind_miss(copy(dt_new), cols = names(.ix))
  .out <- rbindlist(list(.ref, .new))

  expect_equal(as.data.frame(.out),
    data.frame(
      x1 = c(NA, 1, 1),
      x2 = c(2, NA, 2),
      x3 = c(3, 3, NA),
      x1_missing = c(1L, 0L, 0L),
      x2_missing = c(0L, 1L, 0L)
    ))


})




