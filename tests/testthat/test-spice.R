
test_that(
  "soft spicing works",
  {

    data <- data.frame(
      x1 = 1:10,
      x2 = 10:1,
      x3 = 1:10,
      outcome = 11 + runif(10)
    )

    n_miss = 2

    data[1:n_miss, 1:n_miss] = NA

    sft_brew <- brew_soft(data, outcome = outcome)

    expect_error(
      spice(sft_brew, rank_max_init = 1),
      'initial max rank'
    )

    expect_error(
      spice(sft_brew, rank_max_init = 3),
      'initial max rank'
    )

    expect_error(
      spice(sft_brew, rank_stp_size = -5),
      'rank step size'
    )

    expect_error(
      spice(sft_brew, rank_stp_size = 1.5),
      'rank step size'
    )

    expect_error(
      spice(sft_brew, with = spicer_soft(rank_max_ovrl = 3)),
      'overall max rank'
    )

    expect_error(
      spice(sft_brew, with = spicer_soft(rank_max_ovrl = -3)),
      'overall max rank'
    )

    expect_error(
      spice(sft_brew, with = spicer_soft(lambda = -10)),
      'lambda should be'
    )

    sft_brew <- spice(sft_brew, with = spicer_soft())

    expect_true(is_spicer(spicer_soft()))

    expect_equal(sft_brew$pars,
      list(
        rank_max_init = 2L,
        rank_max_ovrl = 2,
        rank_stp_size = 1L,
        lambda = c(
          1.2,
          1.17777777777778,
          1.15555555555556,
          1.13333333333333,
          1.11111111111111,
          1.08888888888889,
          1.06666666666667,
          1.04444444444444,
          1.02222222222222,
          1
        ),
        grid = FALSE
      )

    )

  }
)

test_that(
  "nbrs spicing works",
  {

    data <- data.frame(
      x1 = 1:10,
      x2 = 10:1,
      x3 = 1:10,
      outcome = 11 + runif(10)
    )

    n_miss = 2

    data[1:n_miss, 1:n_miss] = NA

    knn_brew <- brew_nbrs(data, outcome = outcome)

    a <- TRUE

    expect_warning(
      knn_brew <- spice(knn_brew, k_neighbors = 1:10, aggregate = a),
      'neighbor values > 8'
    )

    expect_equal(knn_brew$pars$k_neighbors, 1:8)
    expect_equal(knn_brew$pars$aggregate, a)

    expect_equal(attr(knn_brew, 'n_impute'), 8)

    expect_true(is_spicer(spicer_nbrs()))

    knn_brew_with_spicer <- brew_nbrs(data, outcome = outcome) %>%
      spice(with = spicer_nbrs(k_neighbors = 1:8))

    expect_equal(knn_brew, knn_brew_with_spicer)

    expect_error(
      suppressWarnings(spice(knn_brew, k_neighbors = -1, aggregate = T)),
      'all k_neighbor'
    )

    expect_warning(
      spice(knn_brew, k_neighbors = c(-1, 0, 2), aggregate = T),
      'neighbor values < 1'
    )

    expect_warning(
      spice(knn_brew, k_neighbors = c(2), aggregate = c(T,F)),
      'should have length of 1'
    )

  }
)
