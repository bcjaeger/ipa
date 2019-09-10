
#' midy: a framework for multiple imputation with decision trees
#'   in the tidyverse.
#'
#' midy provides a flexible set of tools to help develop
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
#' To learn more about midy, start with the vignettes:
#' `browseVignettes(package = "midy")`
#'
#' @import purrr
#' @import dplyr
#' @importFrom gower gower_topn
#' @importFrom xgboost xgb.DMatrix xgboost xgb.train xgb.cv
#' @importFrom simsurv simsurv
#' @importFrom recipes prep bake juice recipe step_dummy all_nominal
#'   all_predictors
#' @importFrom tidyr nest unnest
#' @importFrom softImpute softImpute lambda0 complete biScale
#' @importFrom mice ampute ampute.default.patterns
#' @importFrom naniar bind_shadow
#' @importFrom mltools one_hot
#' @importFrom missForest missForest
#' @importFrom gbm basehaz.gbm
#' @importFrom data.table := as.data.table
#' @importFrom glue glue glue_collapse
#' @importFrom tibble enframe as_tibble deframe
#' @importFrom stats median na.omit predict quantile rnorm runif
#'   as.formula model.matrix model.frame
#' @importFrom magrittr %>% %<>% use_series set_colnames divide_by multiply_by
#' @importFrom mvtnorm rmvnorm
#' @importFrom ranger ranger
#' @importFrom VIM kNN gowerD
#'

"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".",
    ".SD",
    "type",
    "role",
    "term",
    "name",
    "pred",
    "data",
    "preds",
    "value",
    "params",
    "level",
    "nimpute",
    "variable",
    "miss_stat",
    "importance",
    "miss_strat",
    "._midy.ID_.",
    "factor_contrast_val"
  )
)
