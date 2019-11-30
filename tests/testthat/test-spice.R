
test_that(
  "good inputs work, bad inputs get good message",
  {

    data <- data.frame(
      x1 = 1:10,
      x2 = 10:1,
      x3 = 1:10,
      outcome = 11 + runif(10)
    )

    n_miss = 2

    data[1:n_miss, 1:n_miss] = NA

    knn_brew <- brew(data, outcome = outcome, flavor = 'kneighbors')
    sft_brew <- brew(data, outcome = outcome, flavor = 'softImpute')
    rgr_brew <- brew(data, outcome = outcome, flavor = 'missRanger')

    expect_error(
      knn_brew <- spice(knn_brew, neighbors = 1:10, aggr_neighbors = T),
      'all neighbor sequence values'
    )

    expect_error(
      knn_brew <- spice(knn_brew, neighbors = -1, aggr_neighbors = T),
      'all neighbor sequence values'
    )

    expect_error(
      spice(sft_brew, n_impute = 5),
      'step_size or n_impute'
    )

    expect_error(
      spice(sft_brew, step_size = 5),
      'step_size or n_impute'
    )

    expect_error(
      spice(sft_brew, step_size = 1.5),
      'step size should be an integer'
    )

    expect_error(
      spice(sft_brew, n_impute = 1.5),
      'number of imputations should be an integer'
    )

    knn_brew_a <- spice(knn_brew, with = spicer_nbrs(neighbors = 1:5))
    knn_brew_b <- spice(knn_brew, neighbors = 1:5, aggr_neighbors = T)

    expect_equivalent(knn_brew_a, knn_brew_b)

    knn_brew <- spice(knn_brew, neighbors = 1:5, aggr_neighbors = T)
    expect_equal(knn_brew$pars$nbrs, 1:5)
    expect_true(all(knn_brew$pars$aggr))

    sft_brew_a <- spice(sft_brew, with = spicer_soft(n_impute = 2))
    sft_brew_b <- spice(sft_brew, n_impute = 2)

    expect_equivalent(sft_brew_a, sft_brew_b)

    sft_brew <- spice(sft_brew, n_impute = 2)

    expect_equal(sft_brew$pars,
      list(
        min_rank = 1,
        max_rank = 2,
        n_impute = 2,
        step_size = 1L
      )
    )

    expect_error(
      spice(rgr_brew, min_node_sizes = c(1:10)),
      regexp = 'all node size values'
    )

    expect_error(
      spice(rgr_brew, min_node_sizes = c(-1)),
      regexp = 'all node size values'
    )

    expect_error(
      spice(rgr_brew, min_node_sizes = c(1:4), pmm_donor_sizes = c(1,2)),
      regexp = 'predictive mean matching donor sequence'
    )


    rgr_brew_a <- spice(rgr_brew, min_node_sizes=1:5, pmm_donor_sizes = 0)
    rgr_brew_b <- spice(rgr_brew, with = spicer_rngr(min_node_sizes = 1:5))

    expect_equivalent(rgr_brew_a, rgr_brew_b)

    rgr_brew <- spice(rgr_brew, min_node_sizes=1:5, pmm_donor_sizes = 1)

    expect_equal(
      rgr_brew$pars,
      list(
        n_impute = 5,
        node_size = 1:5,
        donor_size = c(1, 1, 1, 1, 1)
      )
    )

    rgr_brew <- spice(rgr_brew, min_node_sizes=1:5, pmm_donor_sizes = 1:5)

    expect_equal(
      rgr_brew$pars,
      list(
        n_impute = 5,
        node_size = 1:5,
        donor_size = 1:5
      )
    )
  }
)
