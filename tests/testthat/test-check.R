

data <- as.data.table(diabetes$missing)

test_that('check_chr works',
  {

    input = 'a'
    options = c('b','c')
    expect_null(check_chr('b', label = 'b', options = options))
    expect_error(check_chr(input, label = 'a', options = options))

  }
)


test_that("check_missing works", {


  bad_data <- data
  bad_data$x1 <- NA_real_

  miss_indx <- mindx(bad_data, drop_empty = FALSE)

  expect_error(
    check_missingness(miss_indx, N = nrow(bad_data),
      P = ncol(bad_data), label = 'lab'),
    regexp = 'columns in lab are missing data for all values\\: x1'
  )

  bad_data <- data
  bad_data[1, 2:ncol(data)] <- NA
  miss_indx <- mindx(bad_data, drop_empty = FALSE)
  miss_indx$diabetes <- NULL

  expect_error(
    check_missingness(miss_indx, N = nrow(bad_data),
      P = ncol(bad_data)-1, label = 'lab'),
    regexp = 'rows in lab are missing data for all values\\: 1'
  )

  bad_data <- data
  bad_data[50, 2:ncol(data)] <- NA
  miss_indx <- mindx(bad_data, drop_empty = FALSE)
  miss_indx$diabetes <- NULL

  expect_error(
    check_missingness(miss_indx, N = nrow(bad_data),
      P = ncol(bad_data)-1, label = 'lab'),
    regexp = 'rows in lab are missing data for all values\\: 50'
  )

  bad_data <- data
  bad_data[c(1, 6, 26, 50), 2:ncol(data)] <- NA
  miss_indx <- mindx(bad_data, drop_empty = FALSE)
  miss_indx$diabetes <- NULL

  expect_error(
    check_missingness(miss_indx, N = nrow(bad_data),
      P = ncol(bad_data)-1, label = 'lab'),
    regexp = 'rows in lab are missing data for all values: 1, 6, 26 and 50'
  )


})

test_that('check_type works', {

  x <- numeric(1)

  expect_error(
    check_type(x = x, label = 'lab', type = 'logical'),
    'lab must have type <logical>, but it has type <numeric>'
  )

})

test_that('check_int works', {

  x <- 1.5

  expect_error(check_int(x, label = 'lab'), "lab should be an integer")
  expect_null(check_int(1, label = 'lab'))

})

test_that('check_chr works', {

  x <- 'bad'

  expect_error(
    check_chr(x, label = 'lab', options = c('good', 'better')),
    'lab should be one of good or better'
  )

  expect_null(
    check_chr('good', label = 'lab', options = c('good', 'better'))
  )

})

test_that('check_var_types works', {

  df <- data.frame(a = 1, b = Sys.Date())

  expect_error(
    check_var_types(df, valid_types = 'numeric'),
    '<b> has type <Date>'
  )

  expect_null(
    check_var_types(df, valid_types = c('numeric', 'Date'))
  )

})

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
