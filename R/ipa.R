
#' ipa: a framework for multiple imputation with decision trees
#'   in the tidyverse.
#'
#' ipa provides a flexible set of tools to help develop
#'   ensembles of decision trees using training data that contain
#'   missing values.
#'
#' It has two main goals:
#'
#' \itemize{
#' \item Provide functions to conduct multiple imputation in roughly the
#'   same computational cost as single imputation with common algorithms
#'   (i.e., \code{\link[missForest]{missForest}} and \code{\link[VIM]{kNN}}).
#' \item Provide convenient wrappers for xgboost that allow a simplified
#'   workflow to compare different strategies to handle missing values.
#' }
#'
#' To learn more about ipa, start with the vignettes:
#' `browseVignettes(package = "ipa")`
#'
#' @importFrom rlang %||%
#'
#' @importFrom gower gower_topn
#'
#' @importFrom magrittr %>% %<>%

"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".",
    'fit',
    'name',
    'value',
    '._ID_.',
    'impute',
    'lambda',
    'aggr_fun',
    'variable',
    'node_size',
    'donor_size',
    'k_neighbors'
  )
)

