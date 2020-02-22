
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

    knn_brew <- brew_nbrs(trn, outcome = c) %>%
      spice(with = spicer_nbrs(neighbors = 1:5)) %>%
      mash() %>%
      ferment(tst = test_nbrs(tst))

    brew_dfs <- bottle(knn_brew)

    knn_imps <- impute_knn(
      data_ref = dplyr::select(trn, -c),
      data_new = dplyr::select(tst, -c),
      k = 1:5)

    expect_true(all(brew_dfs$tst[[5]]$b == knn_imps[[5]]$b))

  }
)
