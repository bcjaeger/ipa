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


