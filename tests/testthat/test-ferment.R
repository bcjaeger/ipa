
test_that(
  "soft impute ferment works",
  {

    x1 = rnorm(100)
    x2 = rnorm(100) + x1
    x3 = rnorm(100) + x1 + x2
    x4 = rnorm(100) + x1 + x2 + x3

    outcome = 0.5 * (x1 - x2 + x3 + x4)

    data <- data.frame(x1=x1, x2=x2, x3=x3, x4=x4, outcome=outcome)

    n_miss = 10

    trn = data[1:50, ]
    tst = data[51:100, ]

    trn[1:n_miss,'x1'] = NA
    tst[1:n_miss,'x1'] = NA

    sft_brew <- brew_soft(trn, outcome = outcome, bind_miss = TRUE) %>%
      mash()

    expect_error(
      ferment(sft_brew, data_new = tst[, -2]),
      regexp = 'not contained in new data: x2'
    )

    tmp_new_data <- tst
    tmp_new_data$x5 = rnorm(50)

    expect_error(
      ferment(sft_brew, data_new = tmp_new_data),
      regexp = 'not contained in reference data: x5'
    )

    sft_brew <- sft_brew %>%
      ferment(data_new = tst)

    # knn_brew <- brew_nbrs(trn, outcome = outcome, bind_miss = TRUE) %>%
    #   mash()
    #
    # expect_error(
    #   ferment(knn_brew, data_new = tst[, -2]),
    #   regexp = 'not contained in new data: x2'
    # )
    #
    # tmp_new_data <- tst
    # tmp_new_data$x5 = rnorm(50)
    #
    # expect_error(
    #   ferment(knn_brew, data_new = tmp_new_data),
    #   regexp = 'not contained in reference data: x5'
    # )
    #
    # knn_brew <- knn_brew %>%
    #   ferment(data_new = tst)

  }
)


