
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
    tst[n_miss+1, 'x2'] = NA

    sft_brew <- brew_soft(trn, outcome = outcome, bind_miss = TRUE) %>%
      mash() %>%
      stir(timer = TRUE)

    quick_ferment <- ferment(sft_brew)

    expect_true(attr(quick_ferment, 'fermented_cols') == 'training')
    expect_equal(quick_ferment$wort, sft_brew$wort)

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

    tmp_new_data <- tst
    tmp_new_data$outcome[1] = NA

    expect_error(
      ferment(sft_brew, data_new = tmp_new_data),
      regexp = 'missing values in outcome'
    )

    sft_brew <- sft_brew %>%
      ferment(data_new = tst)

    expect_true(length(sft_brew$miss$training)==1)
    expect_true(length(sft_brew$miss$testing)==2)
    expect_is(sft_brew$wort, 'data.table')

    sft_brew <- sft_brew %>%
      ferment(data_new = as.data.table(tst))

    expect_true(length(sft_brew$miss$training)==1)
    expect_true(length(sft_brew$miss$testing)==2)
    expect_is(sft_brew$wort, 'data.table')

  }
)


