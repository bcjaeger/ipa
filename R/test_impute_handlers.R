
#' Impute test data
#'
#' Some missing data strategies (e.g., soft imputation) are not
#'   designed to impute missing values in testing data. However, the
#'   `softImpute` strategy may still be applied to the testing data
#'   using the same fit parameters as in the imputation model for the
#'   training data. In addition, to enhance the accuracy of test imputation
#'   models, the training data are stacked on top of the testing data
#'   prior to fitting the models.
#'
#' @param data data to be imputed.
#'
#' @param dbl_impute (`TRUE` / `FALSE`) if `TRUE`, then each imputed
#'   training set is stacked on top of the testing data, separately, prior
#'   to fitting each imputation model for the testing data. If `FALSE`,
#'   then the original data is stacked on top of the testing data, and
#'   each imputation model is fitted to the same stacked dataset.
#'
#' @return An object of class 'dbl_imputer'
#'
#' @export
#'
#' @note This function should only be applied within the `ferment`
#'   function. Outside of this context, `test_stkr()` has no purpose.
#'   see ?ferment and vignettes for more detailed examples
#'


test_stkr <- function(data, dbl_impute = TRUE){

  stopifnot(is.logical(dbl_impute))

  structure(
    .Data = list(
      data = data,
      strat = 'stack',
      dbl_impute = dbl_impute
    ),
    class = 'test_imputer'
  )

}



#' Impute test data
#'
#' Some missing data strategies (e.g., soft imputation) are not
#'   designed to impute missing values in testing data. However,
#'   each observation in the testing data can be linked to a set
#'   of nearest neighbors in the training data, and imputation
#'   can then be completed using those nearest neighbors.
#'
#' @param data data to be imputed.
#'
#' @param dbl_impute (`TRUE` / `FALSE`) if `TRUE`, nearest neighbors
#'  are identified in each imputed training set, separately. If `FALSE`,
#'  only the original training data (with missing values) are used to
#'  identify nearest neighbors. Additionally, when `dbl_impute` is `TRUE`,
#'  then the arguments `neighbors` and `aggr_neighbors` below will be
#'  set automatically to match those of each imputed training set.
#'
#' @param neighbors (`integer`) Only relevant if `dbl_impute` = `FALSE`.
#'  The number of nearest neighbors identified for each testing
#'  observation.
#'
#' @param aggr_neighbors (`TRUE` / `FALSE`) Only relevant if
#'  `dbl_impute` = `FALSE`. If `TRUE`, then the nearest neighbor
#'  values are aggregated to form imputations. If `FALSE`, then
#'  a single nearest neighbor's values are sampled randomly.
#'
#' @return An object of class 'dbl_imputer'
#'
#' @export
#'
#' @note This function should only be applied within the `ferment`
#'   function. Outside of this context, `test_nbrs()` has no purpose.
#'   Additionally, when `dbl_impute = TRUE`, both `neighbors` and
#'   `aggr_neighbors` are ignored. See ?ferment and vignettes for
#'   more detailed examples

test_nbrs <- function(
  data,
  dbl_impute = FALSE,
  neighbors = NULL,
  aggr_neighbors = NULL
) {

  if(!is.null(neighbors))
    check_pos_int(neighbors, label = 'number of neighbors')

  if(!is.null(aggr_neighbors))
    stopifnot(is.logical(aggr_neighbors))

  if(!is.null(dbl_impute))
    stopifnot(is.logical(dbl_impute))

  structure(
    .Data = list(
      data = data,
      strat = 'nbrs',
      neighbors = neighbors,
      dbl_impute = dbl_impute,
      aggr_neighbors = aggr_neighbors
    ),
    class = 'test_imputer'
  )

}

is_test_imputer <- function(x) inherits(x, 'test_imputer')

check_test_imputer <- function(x=NULL){

  if(is.null(x)) return(NULL)

  if(!is_test_imputer(x)) stop(
    "expressions passed to ... in ferment should be created using ",
    "test_nbrs() or test_stkr()",
    call. = FALSE
  )

  x

}

