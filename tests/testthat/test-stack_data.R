test_that("stack data runs for correct inputs", {

  data_list <- list(
    df1 = data.frame(x = 1:3, y = letters[1:3], stringsAsFactors=F),
    df2 = data.frame(x = 2:4, y = letters[1:3], stringsAsFactors=F)
  )

  stk1 <- stack_data(data_list, keep_ID = TRUE)

  expect_equal(
    object = stk1,
    expected = structure(
      list(
        ._ID_. = c(1L, 1L, 2L, 2L, 3L, 3L),
        x = c(1L, 2L, 2L, 3L, 3L, 4L),
        y = c("a", "a", "b", "b", "c", "c")
      ),
      row.names = c(NA, -6L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )

  stk1 <- stack_data(data_list, keep_ID = FALSE)

  expect_equal(
    object = stk1,
    expected = structure(
      list(
        x = c(1L, 2L, 2L, 3L, 3L, 4L),
        y = c("a", "a", "b", "b", "c", "c")
      ),
      row.names = c(NA, -6L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )


  lst1 <- list_data(data_list, keep_ID = TRUE)

  expect_equal(object = lst1,
    expected = structure(
      list(
        ._ID_. = c(1L, 2L, 3L, 1L, 2L, 3L),
        x = c(1L, 2L, 3L, 2L, 3L, 4L),
        y = c("a", "b", "c", "a", "b", "c")
      ),
      row.names = c(NA, -6L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )

  lst1 <- list_data(data_list, keep_ID = FALSE)

  expect_equal(object = lst1,
    expected = structure(
      list(
        x = c(1L, 2L, 3L, 2L, 3L, 4L),
        y = c("a", "b", "c", "a", "b", "c")
      ),
      row.names = c(NA, -6L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )

})


test_that("stack data does not run for incorrect inputs", {

  data_list <- list(
    df1 = data.frame(x = 1:2, y = letters[1:2]),
    df2 = data.frame(x = 2:4, y = letters[1:3])
  )

  expect_error(
    object = stack_data(data_list, keep_ID = TRUE),
    regexp = 'same number of rows'
  )

  expect_error(
    object = list_data(data_list, keep_ID = FALSE),
    regexp = 'same number of rows'
  )

  data_list <- list(
    df1 = data.frame(x = 1:2, y = letters[1:2], z = 2:3),
    df2 = data.frame(x = 2:4, y = letters[1:3])
  )

  expect_error(
    object = stack_data(data_list, keep_ID = TRUE),
    regexp = 'same number of columns'
  )

  expect_error(
    object = list_data(data_list, keep_ID = FALSE),
    regexp = 'same number of columns'
  )


})
