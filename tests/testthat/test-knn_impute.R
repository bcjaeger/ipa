
test_that(
  "imputation works",
  {

    df <- tibble::tibble(
      a = 1:100,
      b = a / 10,
      c = a - 10 + runif(100)
    )

    df$b[c(1, 10, 50, 100)] <- NA

    trn <- df[11:100, ]
    tst <- df[1:10, ]

    knn_imps <- impute_knn(
      data_ref = dplyr::select(trn, -c),
      data_new = dplyr::select(tst, -c),
      k = 1:5,
      cols = c(a,b),
      fun_aggr_intg = mean
    )

    # impute neighbor values by hand for tst$b[1]
    expect_equal(knn_imps[[1]]$b[1], mean(trn$b[1]))
    expect_equal(knn_imps[[2]]$b[1], mean(trn$b[1:2]))
    expect_equal(knn_imps[[3]]$b[1], mean(trn$b[1:3]))
    expect_equal(knn_imps[[4]]$b[1], mean(trn$b[1:4]))
    expect_equal(knn_imps[[5]]$b[1], mean(trn$b[1:5]))

    # make sure imputes are the same as brew

    knn_brew <- brew_nbrs(trn, outcome = c) %>%
      spice(with = spicer_nbrs(neighbors = 1:5)) %>%
      mash() %>%
      ferment_fit(new_data = tst) %>%
      bottle()

    expect_true(all(knn_brew$testing[[1]]$b == knn_imps[[1]]$b))
    expect_true(all(knn_brew$testing[[2]]$b == knn_imps[[2]]$b))
    expect_true(all(knn_brew$testing[[3]]$b == knn_imps[[3]]$b))
    expect_true(all(knn_brew$testing[[4]]$b == knn_imps[[4]]$b))
    expect_true(all(knn_brew$testing[[5]]$b == knn_imps[[5]]$b))

    expect_warning(
      impute_knn(data_ref = dplyr::select(tst, -c), k = 20),
      regexp = 'exceeds the number of observed data points'
    )

    knn_imps <- impute_knn(data_ref = dplyr::select(tst, -c), k = 1:2)

    expect_equal(knn_imps[[1]]$b[1], mean(tst$b[2]))
    expect_equal(knn_imps[[2]]$b[1], mean(tst$b[2:3]))

  }
)
