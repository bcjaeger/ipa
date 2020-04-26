
#' imputation for predictive analytics - ipa.
#'
#' ipa provides functions to help you design and use effective
#'   strategies to handle missing data.
#'
#' To learn more about ipa, start with the vignettes:
#' `browseVignettes(package = "ipa")`
#'
#' @importFrom rlang %||%
#' @importFrom stats median na.omit coef predict
#' @import data.table
#'
#'
#'

"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".",
    'fit',
    'name',
    'type',
    'value',
    'score',
    '..cols',
    '..keep',
    '..outcome',
    '..par_cols',
    '..keep_cols',
    '..val_names',
    '..impute_prds',
    'impute',
    'lambda',
    'aggr_fun',
    'variable',
    'n_impute',
    'node_size',
    'donor_size',
    'k_neighbors',
    'rank_max',
    'rank_fit',
    'pars',
    'rank_max_init',
    'rank_max_ovrl',
    'rank_stp_size'
  )
)

