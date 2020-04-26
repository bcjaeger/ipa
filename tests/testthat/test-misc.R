
test_that(
  "inputs work",
  {
    things <- 1:3
    expect_equal(list_things(things),"1, 2 and 3")

    things <- letters[1:2]
    expect_equal(list_things(things),"a and b")

  }
)

test_that('drop empty works', {

  a = list(a = 1, b = integer())

  expect_equal(drop_empty(a), list(a=1))

  b <- list(a = 1, b = 2)

  expect_equal(b, drop_empty(b))

})


test_that('text pillar works', {

  expect_equal(
    text_pillar(lhs = 'a', rhs = 1.2, middle = 'is'),
    '<a> is <1.2>'
  )

})

test_that('df_unique_indx works', {

  vec <- c(0, 1, 1, 2, 4, 4, 4, 5)
  expect_equal(df_unique_indx(vec), c(1, 3, 4, 7, 8))

  vec <- c(1:10)
  expect_equal(vec, df_unique_indx(vec))

})
