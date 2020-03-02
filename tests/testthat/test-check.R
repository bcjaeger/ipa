
test_that('check_chr works',
  {

    input = 'a'
    options = c('b','c')
    expect_null(check_chr('b', label = 'b', options = options))
    expect_error(check_chr(input, label = 'a', options = options))

  }
)

test_that("check_data_new_names gets errors right",
  {

    dref <- data.frame(a=1, b=2)
    dnew <- data.frame(a=3, b=4)
    # nothing should happen
    expect_equal(check_data_new_names(dref, dnew), NULL)

    dref <- data.frame(a=1, b=2, c=1)
    dnew <- data.frame(a=3, b=4)
    # error should be thrown for dref only
    expect_error(check_data_new_names(dref, dnew),
      regexp = 'reference data have columns not contained in new data: c')

    dref <- data.frame(a=1, b=2)
    dnew <- data.frame(a=3, b=4, c=1)
    # error should be thrown for dnew only
    expect_error(check_data_new_names(dref, dnew),
      regexp = 'new data have columns not contained in reference data: c')

    dref <- data.frame(a=1, b=2, d=1)
    dnew <- data.frame(a=3, b=4, c=1)
    # error should be thrown for both
    expect_error(check_data_new_names(dref, dnew), regexp = 'Also, ')

  }
)


test_that("check_data_type gets errors right",
  {

    dref <- data.frame(a=1, b=2)
    dnew <- data.frame(a=3, b=4)
    # nothing should happen
    expect_equal(check_data_new_types(dref, dnew), NULL)

    dref <- data.frame(a=1, b='2')
    dnew <- data.frame(a=3, b=4)
    # error should be thrown for dref only
    expect_error(check_data_new_types(dref, dnew),
      regexp = 'b has type <factor> in reference data and type <numeric>')

    dref <- data.frame(a=1, b=2)
    dnew <- data.frame(a='3', b='4')
    # error should be thrown for dnew only
    expect_error(check_data_new_types(dref, dnew),
      regexp = 'Also, ')

  }
)
