

set.seed(329)
data <- diabetes$missing
split <- sample(nrow(data), nrow(data)/2)

trn <- data[split, ]
tst <- data[-split, ]

sft_brew <- brew_soft(trn, outcome = diabetes, bind_miss = TRUE) %>%
  mash(with = masher_soft(si_maxit = 1000)) %>%
  stir(timer = TRUE) %>%
  ferment(data_new = tst)

sft_tbl <- bottle(copy(sft_brew), type = 'tibble')
sft_mat <- bottle(copy(sft_brew), type = 'matrix')


test_that(
  "good inputs work",
  {

    # ---- tibble soft brew
    expect_true(is_bottled(sft_tbl))

    expect_true(all(sapply(sft_tbl$wort$training, nrow) == nrow(trn)))
    expect_true(all(sapply(sft_tbl$wort$testing, nrow) == nrow(tst)))

    # ---- matrix soft brew
    expect_true(is_bottled(sft_mat))
    expect_is(sft_mat$wort, 'data.table')

    sft_mats <- sft_mat$wort %>%
      tidyr::unnest_wider(training, names_sep = '_') %>%
      tidyr::unnest_wider(testing, names_sep = '_')

    expect_is(sft_mats$training_Y[[1]], 'factor')
    expect_is(sft_mats$training_X[[1]], 'matrix')

    expect_equal(
      ncol(sft_mats$training_X[[1]]),
      ncol(one_hot(dplyr::select(diabetes$missing, -diabetes))) +
        sum(purrr::map_lgl(diabetes$missing, ~any(is.na(.x))))
    )

    expect_equal(
      names(sft_mats),
      c(
        "impute",
        "pars",
        "training_Y",
        "training_X",
        "testing_Y",
        "testing_X"
      )
    )

    # ---- tibble nbrs brew

    knn_brew <- brew_nbrs(trn, outcome = diabetes, bind_miss = FALSE) %>%
      mash() %>%
      stir() %>%
      ferment(data_new = tst) %>%
      bottle(type = 'tibble')

    expect_true(is_bottled(knn_brew))

    expect_true(all(sapply(knn_brew$wort$training, nrow) == nrow(trn)))
    expect_true(all(sapply(knn_brew$wort$testing, nrow) == nrow(tst)))
    expect_true(all(sapply(knn_brew$wort$training, ncol) == ncol(trn)))
    expect_true(all(sapply(knn_brew$wort$testing, ncol) == ncol(tst)))

  }
)
