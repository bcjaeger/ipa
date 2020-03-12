
# tests using diabetes data

data('diabetes', package = 'ipa')

data <- tmp <- as.data.table(diabetes$missing)

tmp$weight_kg <- diabetes$complete$weight_kg

knn_brew <- brew_nbrs(tmp, outcome = c(diabetes, weight_kg))
sft_brew <- brew_soft(data, outcome = diabetes, bind_miss = TRUE)

test_that("inheritance works",{

    expect_is(knn_brew, 'kneighbors_brew')
    expect_is(sft_brew, 'softImpute_brew')
    expect_is(knn_brew, 'ipa_brew')
    expect_is(sft_brew, 'ipa_brew')

})

test_that('printing works', {

  expect_equal(print(knn_brew),
    dplyr::select(data,-diabetes,-weight_kg))

  expect_equal(
    print(sft_brew),
    .bind_miss(copy(data))[, diabetes := NULL]
  )

})

test_that('brew data is a data.table', {

  expect_is(knn_brew$data$training, 'data.table')
  expect_is(sft_brew$data$training, 'data.table')

})

test_that('bind miss is attached', {

  expect_false(get_bind_miss(knn_brew))
  expect_true(get_bind_miss(sft_brew))

})

test_that('flavor is attached', {

  expect_equal(get_flavor(knn_brew), 'kneighbors')
  expect_equal(get_flavor(sft_brew), 'softImpute')

})

test_that('outcome data are stored', {

  expect_equal(get_outcome_name(knn_brew), c('diabetes', 'weight_kg'))
  expect_equal(get_outcome_name(sft_brew), c('diabetes'))

})

test_that('training data are stored', {

  expect_equal(
    get_outcome_trn(knn_brew),
    as.data.table(dplyr::select(diabetes$complete, diabetes, weight_kg))
  )

})

test_that('pars are empty', {

  expect_true(is_empty(knn_brew$pars))
  expect_true(is_empty(sft_brew$pars))

})

