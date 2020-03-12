
x1 = rnorm(100)
x2 = rnorm(100) + x1
x3 = rnorm(100) + x1 + x2

outcome = 0.5 * (x1 - x2 + x3)

n_miss = 10
x1[1:n_miss] <- NA

data <- data.frame(x1=x1, x2=x2, x3=x3, outcome=outcome)

sft_brew <- brew_soft(data, outcome=outcome)
sft_brew <- mash(sft_brew, with = masher_soft(bs = TRUE))
sft_brew <- verbose_on(sft_brew, level = 2)
sft_brew <- stir(sft_brew, timer = TRUE)

tm <- attr(sft_brew, 'stir_time')

test_that("stir() works for softimpute", {
  expect_true(inherits(sft_brew$wort, 'data.table'))
  expect_true(nrow(sft_brew$wort) == length(sft_brew$pars$lambda))
  expect_true(inherits(tm, 'difftime'))
})

knn_brew <- brew_nbrs(data, outcome=outcome) %>%
  mash() %>%
  stir()

tm <- attr(knn_brew, 'stir_time')

test_that("stir() works for kneighbors", {
  expect_true(inherits(knn_brew$wort, 'data.table'))
  expect_true(nrow(knn_brew$wort) == length(knn_brew$pars$k_neighbors))
  expect_true(is.null(tm))
})
