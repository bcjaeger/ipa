
test_that(
  "good inputs work, bad inputs get good messages",
  {

    set.seed(329)

    data <- data.frame(
      x1 = 1:10,
      x2 = 1:10,
      outcome = 1:10 + runif(10)
    )

    new_data <- data.frame(
      x1 = 10:1,
      x2 = 1:10,
      outcome = 1:10 + runif(10)
    )

    data[1, 1] = NA
    new_data[1, 1] = NA


    knn_brew <- brew(data, outcome = outcome, flavor = 'kneighbors')
    sft_brew <- brew(data, outcome = outcome, flavor = 'softImpute')
    rgr_brew <- brew(data, outcome = outcome, flavor = 'missRanger')

    knn_brew <- spice(knn_brew, neighbors = 1:5, aggr_neighbors = T)
    sft_brew <- spice(sft_brew, n_impute = 1)
    rgr_brew <- spice(rgr_brew, min_node_sizes=1, pmm_donor_sizes = 1)

    knn_brew <- mash(knn_brew)
    sft_brew <- mash(sft_brew, masher_soft(scale_data = F))
    rgr_brew <- mash(rgr_brew, masher_rngr(num.trees = 1))

    knn_brew <- ferment(knn_brew)
    sft_brew <- ferment(sft_brew)
    rgr_brew <- ferment(rgr_brew)

    knn_brew <- bottle(knn_brew)
    sft_brew <- bottle(sft_brew)
    rgr_brew <- bottle(rgr_brew)

    expect_is(knn_brew, 'tbl_df')
    expect_is(sft_brew, 'tbl_df')
    expect_is(rgr_brew, 'tbl_df')

    expect_is(knn_brew$training[[1]], 'tbl_df')
    expect_is(sft_brew$training[[1]], 'tbl_df')
    expect_is(rgr_brew$training[[1]], 'tbl_df')

    knn_brew <- brew(data, outcome = outcome, flavor = 'kneighbors')
    sft_brew <- brew(data, outcome = outcome, flavor = 'softImpute')
    rgr_brew <- brew(data, outcome = outcome, flavor = 'missRanger')

    knn_brew <- spice(knn_brew, neighbors = 1:5, aggr_neighbors = T)
    sft_brew <- spice(sft_brew, n_impute = 1)
    rgr_brew <- spice(rgr_brew, min_node_sizes=1, pmm_donor_sizes = 1)

    knn_brew <- mash(knn_brew)
    sft_brew <- mash(sft_brew, masher_soft(scale_data = F))
    rgr_brew <- mash(rgr_brew, masher_rngr(num.trees = 1))

    knn_brew <- ferment(knn_brew, new = test_nbrs(new_data))
    sft_brew <- ferment(sft_brew, new = test_nbrs(new_data))
    rgr_brew <- ferment(rgr_brew, new = test_nbrs(new_data))

    knn_brew <- bottle(knn_brew, type = 'matrix')
    sft_brew <- bottle(sft_brew, type = 'matrix')
    rgr_brew <- bottle(rgr_brew, type = 'matrix')

    expect_is(sft_brew$new[[1]]$X, 'matrix')
    expect_is(sft_brew$new[[1]]$Y, 'matrix')

    expect_is(rgr_brew$new[[1]]$X, 'matrix')
    expect_is(rgr_brew$new[[1]]$Y, 'matrix')

    expect_is(knn_brew$new[[1]]$X, 'matrix')
    expect_is(knn_brew$new[[1]]$Y, 'matrix')

  }
)
