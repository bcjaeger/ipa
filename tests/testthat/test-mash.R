
test_that(
  "soft mash works",
  {

    x1 = rnorm(100)
    x2 = rnorm(100) + x1
    x3 = rnorm(100) + x1 + x2

    outcome = 0.5 * (x1 - x2 + x3)

    data <- data.frame(x1=x1, x2=x2, x3=x3, outcome=outcome)

    n_miss = 10

    data[1:n_miss,'x1'] = NA

    expect_message(
      soft_brew <- brew_soft(data, outcome=outcome, bind_miss = TRUE) %>%
        verbose_on(level = 1) %>%
        mash(),
      'Default spices are being used:'
    )

    expect_true(is_mashed(soft_brew))

    # make sure my brew's answer is the same as softImpute's answer
    set.seed(1)
    soft_brew <- brew_soft(data, outcome=outcome, bind_miss = FALSE) %>%
      mash(with = masher_soft(bs = TRUE)) %>%
      stir()

    x <- softImpute::biScale(x = as.matrix(data[, 1:3]),
      row.scale = FALSE, row.center = FALSE)

    set.seed(1)
    fit <- softImpute::softImpute(x, rank.max = 2,
      lambda = soft_brew$pars$lambda[1])

    imputed_x1 <- softImpute::complete(x, fit)[1:n_miss, 'x1']

    expect_true(
      all(soft_brew$wort$imputed_values[[1]]$x1 - imputed_x1 < 1e-5)
    )
  }
)



test_that(
  "nbrs mash works",
  {

    x1 = rnorm(100)
    x2 = rnorm(100) + x1
    x3 = rnorm(100) + x1 + x2

    outcome = 0.5 * (x1 - x2 + x3)

    data <- data.frame(x1=x1, x2=x2, x3=x3, outcome=outcome)

    n_miss = 10

    data[1:n_miss,'x1'] = NA

    expect_message(
      soft_brew <- brew_nbrs(data, outcome=outcome, bind_miss = TRUE) %>%
        verbose_on(level = 1) %>%
        mash(),
      'Default spices are being used:'
    )

  }
)


