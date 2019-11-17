
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

    knn_brew <- verbose_on(knn_brew, level = 2)
    sft_brew <- verbose_on(sft_brew, level = 2)
    rgr_brew <- verbose_on(rgr_brew, level = 2)

    knn_brew <- spice(knn_brew, neighbors = 1:5, aggr_neighbors = T)
    sft_brew <- spice(sft_brew, n_impute = 1)
    rgr_brew <- spice(rgr_brew, min_node_sizes=1, pmm_donor_sizes = 1)

    knn_brew <- mash(knn_brew)
    sft_brew <- mash(sft_brew, masher_soft(scale_data = F))
    rgr_brew <- mash(rgr_brew, masher_rngr(num.trees = 1))

    tmp_new_data = new_data
    tmp_new_data$x3 = 10:1

    expect_error(
      ferment(knn_brew, new = new_data[, 1, drop = F]),
      regexp = 'not contained in new data: x2'
    )

    expect_error(
      ferment(knn_brew, new = tmp_new_data),
      regexp = 'not contained in brew data: x3'
    )


    knn_brew <- ferment(knn_brew)
    sft_brew <- ferment(sft_brew)
    rgr_brew <- ferment(rgr_brew)

    expect_equal(nrow(knn_brew$wort), 5)
    expect_equal(nrow(sft_brew$wort), 1)
    expect_equal(nrow(rgr_brew$wort), 1)

    knn_brew <- brew(data, outcome = outcome, flavor = 'kneighbors')
    sft_brew <- brew(data, outcome = outcome, flavor = 'softImpute')
    rgr_brew <- brew(data, outcome = outcome, flavor = 'missRanger')

    knn_brew <- verbose_on(knn_brew, level = 2)
    sft_brew <- verbose_on(sft_brew, level = 2)
    rgr_brew <- verbose_on(rgr_brew, level = 2)

    knn_brew <- spice(knn_brew, neighbors = 1:5, aggr_neighbors = T)
    sft_brew <- spice(sft_brew, n_impute = 1)
    rgr_brew <- spice(rgr_brew, min_node_sizes=1, pmm_donor_sizes = 1)

    knn_brew <- mash(knn_brew)
    sft_brew <- mash(sft_brew, masher_soft(scale_data = F))
    rgr_brew <- mash(rgr_brew, masher_rngr(num.trees = 1))

    knn_brew <- ferment(knn_brew, new = new_data, dbl_impute = TRUE)
    sft_brew <- ferment(sft_brew, new = new_data, dbl_impute = TRUE)
    rgr_brew <- ferment(rgr_brew, new = new_data, dbl_impute = TRUE)

    knn_brew <- brew(data, outcome = outcome, flavor = 'kneighbors')
    sft_brew <- brew(data, outcome = outcome, flavor = 'softImpute')
    rgr_brew <- brew(data, outcome = outcome, flavor = 'missRanger')

    knn_brew <- verbose_on(knn_brew, level = 2)
    sft_brew <- verbose_on(sft_brew, level = 2)
    rgr_brew <- verbose_on(rgr_brew, level = 2)

    knn_brew <- spice(knn_brew, neighbors = 1:5, aggr_neighbors = T)
    sft_brew <- spice(sft_brew, n_impute = 1)
    rgr_brew <- spice(rgr_brew, min_node_sizes=1, pmm_donor_sizes = 1)

    knn_brew <- mash(knn_brew)
    sft_brew <- mash(sft_brew, masher_soft(scale_data = F))
    rgr_brew <- mash(rgr_brew, masher_rngr(num.trees = 1))

    knn_brew <- ferment(knn_brew, testing = new_data)
    sft_brew <- ferment(sft_brew, testing = new_data)
    rgr_brew <- ferment(rgr_brew, testing = new_data)

    expect_equal(knn_brew$wort$training[[1]]$x1[1], 2L)
    expect_equal(knn_brew$wort$testing[[1]]$x1[1], 9L)

    expect_true(sft_brew$wort$training[[1]]$x1[1] < 1)
    expect_true(sft_brew$wort$testing[[1]]$x1[1] > 5)

    expect_true(rgr_brew$wort$training[[1]]$x1[1] < 5)
    expect_true(rgr_brew$wort$testing[[1]]$x1[1] > 5)

    # no missing values in new data

    new_data <- data.frame(
      x1 = 10:1,
      x2 = 1:10,
      outcome = 1:10 + runif(10)
    )


    knn_brew <- brew(data, outcome = outcome, flavor = 'kneighbors')
    sft_brew <- brew(data, outcome = outcome, flavor = 'softImpute')
    rgr_brew <- brew(data, outcome = outcome, flavor = 'missRanger')

    knn_brew <- verbose_on(knn_brew, level = 2)
    sft_brew <- verbose_on(sft_brew, level = 2)
    rgr_brew <- verbose_on(rgr_brew, level = 2)

    knn_brew <- spice(knn_brew, neighbors = 1:5, aggr_neighbors = T)
    sft_brew <- spice(sft_brew, n_impute = 1)
    rgr_brew <- spice(rgr_brew, min_node_sizes=1, pmm_donor_sizes = 1)

    knn_brew <- mash(knn_brew)
    sft_brew <- mash(sft_brew, masher_soft(scale_data = F))
    rgr_brew <- mash(rgr_brew, masher_rngr(num.trees = 1))

    expect_error(
      ferment(knn_brew, new_data),
      regexp = 'must be named'
    )

    knn_brew <- ferment(knn_brew, testing = new_data)
    sft_brew <- ferment(sft_brew, testing = new_data)
    rgr_brew <- ferment(rgr_brew, testing = new_data)

    expect_true(is_fermented(knn_brew))

    expect_equal(
      as.matrix(knn_brew$wort$testing[[1]]),
      as.matrix(new_data[,c('x1','x2')])
    )

    expect_equal(
      as.matrix(rgr_brew$wort$testing[[1]]),
      as.matrix(new_data[,c('x1','x2')])
    )

    expect_equal(
      as.matrix(sft_brew$wort$testing[[1]]),
      as.matrix(new_data[,c('x1','x2')])
    )



  }
)