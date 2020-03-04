test_that("verbosity works", {

  data <- data.frame(
    x1 = 1:10,
    x2 = 10:1,
    x3 = 1:10,
    outcome = 11 + runif(10)
  )

  n_miss = 2

  data[1:n_miss, 1:n_miss] = NA

  expect_error(verbose_on(1, level = 1), 'not an ipa_brew')

  expect_error(
    verbose_on(brew_nbrs(data, outcome = outcome), level = 3),
    'level should be'
  )

  knn_brew <- verbose_on(brew_nbrs(data, outcome = outcome), level = 2)

  expect_equal(get_verbosity(knn_brew), 2)

  knn_brew <- verbose_off(knn_brew)

  expect_equal(get_verbosity(knn_brew), 0)


})
