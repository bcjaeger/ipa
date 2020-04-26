

data("diabetes")

diabetes$missing <- diabetes$missing %>%
  dplyr::mutate(
    time = runif(n = nrow(.), min = 1, max = 10),
    status = rbinom(n = nrow(.), size = 1, prob = 1/2)
  )

trn <- diabetes$missing[1:200, ]
tst <- diabetes$missing[-c(1:200), ]

imputes <- brew_soft(trn, outcome = c(diabetes, time, status)) %>%
  mash(with = masher_soft(si_maxit = 1000)) %>%
  stir() %>%
  ferment(data_new = tst) %>%
  bottle() %>%
  purrr::pluck('wort') %>%
  dplyr::slice(5)


test_that("scrimp_vars() works, real data", {

  check_cols <- c('diabetes', 'chol', 'height_cm')

  score_vars <- imputes$training[[1]][, check_cols] %>%
    scrimp_vars(data_complete = diabetes$complete[1:200, check_cols],
      data_missing = diabetes$missing[1:200, check_cols])

  expect_is(score_vars, 'data.table')
  expect_equal(nrow(score_vars), nrow(score_vars))

})

# use the default glmnet logistic regression model
dflt_output <- scrimp_mdl(
  train_imputed = imputes$training[[1]],
  test_imputed  = imputes$testing[[1]],
  outcome = diabetes)

test_that('binary predictions are probabilities', {
  expect_true(all(dflt_output$preds >= 0) && all(dflt_output$preds <= 1))
})

test_that('default output is a list', {
  expect_is(dflt_output, 'list')
})

# use default output for glmnet multinomial model

dflt_output <- scrimp_mdl(
  train_imputed = imputes$training[[1]],
  test_imputed  = imputes$testing[[1]],
  outcome = frame,
  .fun_args = net_args(cmplx = 'lambda.min', keep_mdl = TRUE, nfold = 3)
)

test_that('model is returned if asked for', {
  expect_is(dflt_output$model, 'cv.glmnet')
})

test_that('a multinomial model was fitted', {
  expect_equal(dflt_output$model$name, c('deviance' = 'Multinomial Deviance'))
})

# use default output for continuous variable (glmnet gaussian model)

dflt_output <- scrimp_mdl(
  train_imputed = imputes$training[[1]],
  test_imputed  = imputes$testing[[1]],
  outcome = height_cm,
  .fun_args = net_args(cmplx = 'lambda.1se', alpha = 1)
)

test_that("height r-squared is correct", {
  expect_equal(
    as.numeric(cor(dflt_output$preds_ex, imputes$testing[[1]]$height_cm)^2),
    dflt_output$score
  )
})

# use default output for integer variable (glmnet gaussian model)

dflt_output <- scrimp_mdl(
  train_imputed = imputes$training[[1]],
  test_imputed  = imputes$testing[[1]],
  outcome = sbp,
  .fun_args = net_args(cmplx = 'lambda.1se', alpha = 1)
)

test_that("sbp r-squared is correct", {
  expect_equal(
    as.numeric(cor(dflt_output$preds_ex, imputes$testing[[1]]$sbp)^2),
    dflt_output$score
  )
})

dflt_output <- scrimp_mdl(
  train_imputed = imputes$training[[1]],
  test_imputed  = imputes$testing[[1]],
  outcome = c(time, status),
  .fun_args = net_args(cmplx = 'lambda.1se', keep_mdl = TRUE)
)

test_that('model is returned for family = cox', {
  expect_is(dflt_output$model, 'cv.glmnet')
})

test_that('a multinomial model was fitted', {
  expect_equal(dflt_output$model$name,
    c('deviance' = 'Partial Likelihood Deviance'))
})

imputes$training[[1]] <- dplyr::rename(imputes$training[[1]],
  time_stroke = time, status_stroke = status)

imputes$testing[[1]] <- dplyr::rename(imputes$testing[[1]],
  time_stroke = time, status_stroke = status)

test_that('argument name warning goes off', {
  expect_warning(scrimp_mdl(
    train_imputed = imputes$training[[1]],
    test_imputed  = imputes$testing[[1]],
    outcome = dplyr::ends_with('stroke'),
    .fun_args = net_args(cmplx = 'lambda.1se', keep_mdl = TRUE)
  ),
  "outcomes should be named")
})



# write your own function:
# note the function inputs can be ordered however you like, but the
# names of the inputs **must** include be .trn, .tst, and outcome
# rngr_fun <- function(outcome, .trn, .tst, num_trees){
#
#   # make a model formula
#   formula <- as.formula(paste(outcome, '~ .'))
#   # fit a random forest with ranger (probability = TRUE -> predicted probs)
#   mdl <- ranger::ranger(formula = formula, data = .trn,
#     probability = TRUE, num.trees = num_trees)
#   # prediction from ranger returns matrix with two columns, we need the 2nd.
#   prd <- predict(mdl, data = .tst)$predictions[ , 2, drop = TRUE]
#   # compute model AUC
#   yardstick::roc_auc_vec(.tst[[outcome]], prd)
#
# }
#
# scrimp_mdl(
#   train_imputed = imputes$training[[1]],
#   test_imputed  = imputes$testing[[1]],
#   outcome = 'diabetes',
#   .fun = rngr_fun,
#   .fun_args = list(num_trees = 100)
# )


test_that("scrimp_vars() works, artificial data", {

  df_complete <- data.frame(a = 1:10, b = 1:10, c = 1:10, d=1:10,
    fctr = letters[c(1,1,1,1,1,2,2,2,2,2)])

  df_miss = df_complete
  df_miss[1:3, 1] <- NA
  df_miss[2:4, 2] <- NA
  df_miss[3:5, 3] <- NA
  df_miss[4:6, 5] <- NA


  imputes <- list(a=1:3, b=2:4, c=3:5, fctr = factor(c('a','a','b')))

  df_imputed <- fill_na(df_miss, vals = imputes)

  scored <- scrimp_vars(df_imputed, df_miss, df_complete)

  expect_true(all(na.omit(scored$score) == 1))
  expect_true(all(scored$type[1:3] == 'intg'))
  expect_true(scored$type[scored$variable=='fctr'] == 'bnry')


})


