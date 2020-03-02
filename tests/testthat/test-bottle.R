
test_that(
  "good inputs work, bad inputs get good messages",
  {

    x1 = rnorm(100)
    x2 = rnorm(100) + x1
    x3 = rnorm(100) + x1 + x2
    x4 = rnorm(100) + x1 + x2 + x3

    outcome = 0.5 * (x1 - x2 + x3 + x4)

    data <- data.frame(x1=x1, x2=x2, x3=x3, x4=x4, outcome=outcome)

    n_miss = 10

    trn = data[1:40, ]
    tst = data[41:100, ]

    trn[1:n_miss,'x1'] = NA
    tst[1:n_miss,'x1'] = NA

    # ---- tibble soft brew

    sft_brew <- brew_soft(trn, outcome = outcome, bind_miss = TRUE) %>%
      mash() %>%
      ferment(data_new = tst) %>%
      bottle(type = 'tibble')

    expect_true(is_bottled(sft_brew))
    expect_is(sft_brew$wort, 'tbl_df')

    expect_true(all(sapply(sft_brew$wort$training, nrow) == nrow(trn)))
    expect_true(all(sapply(sft_brew$wort$testing, nrow) == nrow(tst)))
    expect_true(all(names(trn) %in% names(sft_brew$wort$training[[1]])))
    expect_true(all(names(tst) %in% names(sft_brew$wort$testing[[1]])))

    # ---- matrix soft brew

    sft_mats <- brew_soft(trn, outcome = outcome, bind_miss = TRUE) %>%
      mash() %>%
      ferment(data_new = tst) %>%
      bottle(type = 'matrix') %>%
      purrr::pluck('wort') %>%
      tidyr::unnest_wider(training, names_sep = '_') %>%
      tidyr::unnest_wider(testing, names_sep = '_')

    expect_equal(
      names(sft_mats),
      c(
        "impute",
        "pars",
        "training_X",
        "training_Y",
        "testing_X",
        "testing_Y"
      )
    )

    expect_is(sft_mats$training_X[[1]], 'matrix')
    expect_is(sft_mats$training_Y[[1]], 'matrix')

    # ---- tibble nbrs brew

    # knn_brew <- brew_nbrs(trn, outcome = outcome, bind_miss = FALSE) %>%
    #   mash() %>%
    #   ferment(data_new = tst) %>%
    #   bottle(type = 'tibble')
    #
    # expect_true(is_bottled(knn_brew))
    # expect_is(knn_brew$wort, 'tbl_df')
    #
    # expect_true(all(sapply(knn_brew$wort$training, nrow) == nrow(trn)))
    # expect_true(all(sapply(knn_brew$wort$testing, nrow) == nrow(tst)))
    # expect_true(all(sapply(knn_brew$wort$training, ncol) == ncol(trn)))
    # expect_true(all(sapply(knn_brew$wort$testing, ncol) == ncol(tst)))

  }
)
