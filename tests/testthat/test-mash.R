
test_that(
  "good inputs work, bad inputs get good messages",
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

    knn_brew <- spice(knn_brew, neighbors = 1:5, aggr_neighbors = T)
    sft_brew <- spice(sft_brew, n_impute = 2)
    rgr_brew <- spice(rgr_brew, min_node_sizes=1:5, pmm_donor_sizes = 0)

    knn_brew <- mash(knn_brew)

    expect_error(
      mash(sft_brew, scale_data = 1),
      regexp = 'should be a single logical'
    )

    expect_error(
      mash(sft_brew, scale_data = T),
      regexp = 'unable to run biScale on brew data'
    )

    sft_brew <- mash(sft_brew, scale_data = F)

    rgr_brew <- mash(rgr_brew)

    sft_fits <- sft_brew$wort$fit

    for(i in seq_along(sft_fits)){
      expect_equal(names(sft_fits[[i]]), c('u','d','v'))
    }


    fit_rows <- sapply(knn_brew$wort$fit, function(x) nrow(na.omit(x)))
    expect_true(all(fit_rows == nrow(data)))

    fit_rows <- sapply(rgr_brew$wort$fit, function(x) nrow(na.omit(x)))
    expect_true(all(fit_rows == nrow(data)))


  }
)
