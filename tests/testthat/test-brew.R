
test_that(
  "correct inputs work and incorrect inputs get correct message",
  {

    data <- data.frame(
      x1 = 1:10,
      x2 = 10:1,
      x3 = 1:10,
      outcome = 11 + runif(10)
    )

    n_miss = 2

    data[1:n_miss, 1:n_miss] = NA

    bad_data <- data
    bad_data[, 1] = NA

    expect_error(
      brew(bad_data, outcome = outcome),
      regexp = 'columns are missing'
    )

    bad_data <- data
    bad_data[1, ] = NA

    expect_error(
      brew(bad_data, outcome = outcome),
      regexp = 'rows are missing'
    )

    knn_brew <- brew(data, outcome = outcome, flavor = 'kneighbors')
    sft_brew <- brew(data, outcome = outcome, flavor = 'softImpute')
    rgr_brew <- brew(data, outcome = outcome, flavor = 'missRanger')

    expect_true(!attr(knn_brew, 'bind_miss'))

    expect_error(
      brew(data, outcome = outcome, flavor = 'kneighbor'),
      regexp = 'flavor should be'
    )

    bad_data <- data
    bad_data$letter <- letters[1:10]

    expect_error(
      brew(bad_data, outcome = outcome, flavor = 'kneighbors'),
      regexp = 'Unsupported variable types'
    )

    bad_data$letter = factor(bad_data$letter)

    expect_error(
      brew(bad_data, outcome = outcome, flavor = 'softImpute'),
      regexp = 'Unsupported variable types'
    )

    expect_true(length(knn_brew$pars)==0)
    expect_true(length(sft_brew$pars)==0)
    expect_true(length(rgr_brew$pars)==0)

    expect_is(knn_brew, 'kneighbors_brew')
    expect_is(sft_brew, 'softImpute_brew')
    expect_is(rgr_brew, 'missRanger_brew')

    max_obs <- nrow(data) - n_miss
    expect_equal(knn_brew$lims$neighbors$min, 1)
    expect_equal(knn_brew$lims$neighbors$max, max_obs)

    expect_null(knn_brew$wort)
    expect_null(sft_brew$wort)
    expect_null(rgr_brew$wort)

    prt = print(knn_brew)

    expect_is(prt, 'tbl_df')

    knn_brew <- data %>%
      brew(outcome = outcome, flavor = 'kneighbors', bind_miss = T)

    expect_equal(
      names(knn_brew$data),
      c("x1", "x2", "x3", "x1_missing", "x2_missing", "x3_missing")
    )

    expect_true(attr(knn_brew, 'bind_miss'))


  }
)

