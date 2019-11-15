test_that(
  "inputs work",
  {
    things <- 1:3
    expect_equal(list_things(things),"1, 2 and 3")

    things <- letters[1:2]
    expect_equal(list_things(things),"a and b")

  }
)
