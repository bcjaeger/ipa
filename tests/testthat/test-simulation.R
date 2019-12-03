
test_that(
  "good inputs work, bad inputs get good errors",
  {

    set.seed(1)
    regr = gen_simdata(problem_type = 'regression',
      ncov = 3, nint = 2, degree = 3, nobs = 2000,
      tst_miss_prop = 0)

    expect_true(length(regr$beta) == 11L)

    set.seed(1)
    clsf = gen_simdata(problem_type = 'classification',
      ncov = 3, nint = 2, degree = 3, nobs = 2000)

    expect_true(length(clsf$beta) == 11L)

    set.seed(1)
    surv = gen_simdata(problem_type = 'survival',
      ncov = 3, nint = 2, degree = 3, nobs = 2000)

    expect_true(length(surv$beta) == 11L)

    expect_true(all(regr$beta == clsf$beta))
    expect_true(all(clsf$beta == surv$beta))

    expect_error(
      gen_simdata(problem_type = 'regression',
        ncov = 3, nint = 30, degree = 3),
      regexp = 'maximum'
    )

  amputed_dat <- regr$testing %>%
    add_missing(omit_cols = 'response', miss_proportion = 1/2,
      miss_pattern = 'mar', npatterns = 10)

  expect_true(!any(is.na(amputed_dat$response)))

  expect_true(sum(is.na(amputed_dat$x2))>0)

  expect_true(sum(is.na(amputed_dat$x3))>0)


  }
)
