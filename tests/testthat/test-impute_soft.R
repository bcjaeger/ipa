
n <- 10

DT_orig <- DT <- data.table::data.table(
  V1 = seq(n),
  V2 = factor(letters[c(3,3,3,2,2,2,1,1,1,1)]),
  V3 = seq(n) / 10,
  V4 = letters[rep(c(1,2), each = 5)]
)

DT$V1[1] <- NA
DT$V2[1:2] <- NA
DT$V3[c(6,7)] <- NA
DT$V4[2] <- NA

DT_new <- data.table::data.table(
  V1 = seq(n),
  V2 = factor(letters[c(3,3,3,3,2,2,1,2,1,1)]),
  V3 = seq(n) / 10,
  V4 = letters[rep(c(1,3), each = 5)]
)

DT_new$V1[2] <- NA
DT_new$V2[3:5] <- NA
DT_new$V3[c(1,6)] <- NA
DT_new$V4[9] <- NA

imps_ref <- impute_soft(DT)
imps_ref_1hot <- impute_soft(DT, restore_data = FALSE)
imps_ref_grid <- impute_soft(DT, restore_data = FALSE, grid = TRUE)

imputed_types_1hot <- purrr::map_chr(
  .x = imps_ref_1hot$imputed_values[[1]],
  .f = get_var_type
)

imputed_types <- purrr::map_chr(
  .x = imps_ref$imputed_values[[1]],
  .f = get_var_type
)

test_that('data restoration maintains variable types', {
  expect_equal(
    imputed_types,
    c(V1 = "intg", V3 = "ctns", V2 = "catg", V4 = "bnry")
  )
})

test_that('unrestored data is strictly continuous', {
  expect_true(all(imputed_types_1hot == 'ctns'))
})

imps_new <- impute_soft(DT, data_new = DT_new)

imps_new_1hot <- impute_soft(DT, data_new = DT_new, restore_data = FALSE)

test_that("imputations works when new data have new factor levels", {

  lvls_v4 <- levels(imps_new$imputed_values[[1]]$V4)

  expect_equal(lvls_v4, letters[1:3])

})

test_that("unrestored data match the names of one hot encoded data", {
  expect_equal(
    names(one_hot(rbindlist(list(DT, DT_new)))),
    names(imps_new_1hot$imputed_values[[4]])
  )
})

test_that('error is thrown if there is only one column selected', {
  expect_error(impute_soft(DT, cols = V1), '2\\+ are needed')
})

test_that('warning is given if data have no missing values', {
  expect_warning(impute_soft(DT_orig), regexp = 'no missing values')
})


test_that('grid will give the same lambda seq / rank', {

  total_combos <- length(unique(imps_ref_grid$lambda)) *
    length(unique(imps_ref_grid$rank_max))

  expect_equal(total_combos, nrow(imps_ref_grid))

})

