
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
      regexp = 'x1'
    )

    bad_data <- data
    bad_data[1, 1:3] = NA

    expect_error(
      brew(bad_data, outcome = outcome),
      regexp = 'rows in data_ref'
    )

    knn_brew <- brew(data, outcome = outcome, flavor = 'kneighbors')
    sft_brew <- brew(data, outcome = outcome, flavor = 'softImpute')

    knn_brew_bm <- brew_nbrs(data, outcome = outcome, bind_miss = TRUE)
    sft_brew_bm <- brew_soft(data, outcome = outcome, bind_miss = TRUE)


    expect_error(
      brew(data, outcome = outcome, flavor = 'kneighbor'),
      regexp = 'flavor should be'
    )

    bad_data <- data
    bad_data$letter <- letters[1:10]

    expect_error(
      brew(bad_data, outcome = outcome, flavor = 'kneighbors'),
      regexp = '<letter> has type <character>'
    )

    bad_data$letter = factor(bad_data$letter)

    expect_true(length(knn_brew$pars)==0)
    expect_true(length(sft_brew$pars)==0)

    expect_is(knn_brew, 'kneighbors_brew')
    expect_is(sft_brew, 'softImpute_brew')

    max_obs <- nrow(data) - n_miss
    expect_equal(knn_brew$lims$neighbors$min, 1)
    expect_equal(knn_brew$lims$neighbors$max, max_obs)


    expect_equal(
      names(knn_brew_bm$data$training),
      c("x1", "x2", "x3")
    )

    expect_equal(
      names(knn_brew_bm$miss$training),
      c("x1_missing", "x2_missing")
    )

  }
)

