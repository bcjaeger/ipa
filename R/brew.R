

#' New brew
#'
#' `brew()` is the first function in a workflow that the `ipa` package
#'   tries to simplify. You may be wondering whether `ipa` stands for
#'   'imputation for predictive analytics' or 'india pale ale', given
#'   this ambiguous function name. Perhaps it can be both?
#'
#' Brewing a delicious beer is not that different from imputing
#'   missing data. Once a brew is started, you can add spices
#'   (set tuning parameters; see [spice]) or just go right to mashing
#'   (fitting imputation models; see [mash]). To finish the brew, add
#'   yeast (new data; see [ferment]), and then bottle it up (see [bottle])
#'   as a `tibble` or `matrix`.
#'
#'   `brew()` includes an input variable called `flavor` that determines
#'     how data will be imputed.  `brew_nbrs()`, `brew_rngr()`, and
#'     `brew_soft()` are convenience functions, e.g. `brew_nbrs()` is a
#'     shortcut for calling `brew(flavor = 'kneighbors')`.
#'
#' @param data a data frame with missing values.
#'
#' @param outcome column name(s) of outcomes in `data`.
#'   These values can be provided as symbols (e.g., outcome = c(a,b,c)
#'   for multiple outcomes or outcome = a for one outcome) or
#'   character values (e.g., outcome = c('a','b','c') for multiple outcomes
#'   or outcome = 'a' for a single outcome).
#'
#' @param bind_miss (`TRUE` / `FALSE`). If `TRUE`, a set of additional
#'   indicator columns (one for each non-outcome column) are added
#'   to `data`. The indicator columns take values of 0 and 1, with
#'   0 indicating that this variable is not missing for this row
#'   and 1 indicating that this variable is missing for this row.
#'   If `FALSE`, no additional columns are added to `data`.
#'
#' @param flavor the computational approach that will be used to
#'   impute missing data. Valid options are 'kneighbors', 'softImpute',
#'   and 'missRanger.' These values should be input as characters
#'   (e.g., 'kneighbors').
#'
#' @return an `ipa_brew` object with your specified `flavor`
#'
#' @section **Neighbor's brew**: an adaptation of Max Kuhn's
#'  nearest neighbor imputation functions in the [recipes][recipes::recipes]
#'  and `caret` packages. It also uses the [gower][gower::gower_topn]
#'  package to implement algorithms that compute Gower's distance.
#'
#'  What makes this type of nearest neighbor imputation
#'  different is its flexibility in the number of neighbors used
#'  to impute missing values and the aggregation function applied.
#'  For example, to create 10 imputed datasets that use 1, 2, ..., 10
#'  neighbors to impute missing values would require fitting
#'  10 separate nearest neighbors models using conventional functions.
#'  The `ipa` package lets a user create all of these imputed sets
#'  with just one fitting of a nearest neighbor model. Additionally,
#'  for users who want to use nearest neighbors for multiple imputation,
#'  `ipa` gives the option to sample 1 neighbor value at random from
#'  a neighborhood, rather than aggregate values into a summary.
#'
#' @section **Soft brew**: The `softImpute` algorithm is used to impute
#'   missing values with this `brew`. For more details on this strategy
#'   to handle missing values, please see
#'   [softImpute][softImpute::softImpute()].
#'
#' @section **Ranger's brew**: The `missRanger` algorithm is used to
#'   impute missing values with the `brew`. This strategy is very similar
#'   to the algorithm proposed by Stekhoven (2012). For more details
#'   on this strategy  to handle missing values, please see
#'   [missRanger][missRanger::missRanger()].
#'
#' @note Gower (1971) originally defined a similarity measure (s, say) with
#' values ranging from 0 (completely dissimilar) to 1 (completely similar).
#' The distance returned here equals 1-s.
#'
#' @references Gower, John C. "A general coefficient of similarity
#'  and some of its properties." Biometrics (1971): 857-871.
#'
#' Rahul Mazumder, Trevor Hastie and Rob Tibshirani (2010)
#' Spectral Regularization Algorithms for Learning Large Incomplete
#' Matrices, http://www.stanford.edu/~hastie/Papers/mazumder10a.pdf
#' *Journal of Machine Learning Research* 11 (2010) 2287-2322
#'
#' Stekhoven, D.J. and Buehlmann, P. (2012), 'MissForest - nonparametric
#'  missing value imputation for mixed-type data', Bioinformatics, 28(1)
#'  2012, 112-118, doi: 10.1093/bioinformatics/btr597
#'
#' @export
#'
#' @examples
#'
#' data <- data.frame(
#'   x1 = 1:10,
#'   x2 = 10:1,
#'   x3 = 1:10,
#'   outcome = 11 + runif(10)
#' )
#'
#' data[1:2, 1:2] = NA
#'
#' knn_brew <- brew(data, outcome = outcome, flavor = 'kneighbors')
#' sft_brew <- brew(data, outcome = outcome, flavor = 'softImpute')
#' rgr_brew <- brew(data, outcome = outcome, flavor = 'missRanger')
#'
#' knn_brew <- brew_nbrs(data, outcome = outcome)
#' sft_brew <- brew_soft(data, outcome = outcome)
#' rgr_brew <- brew_rngr(data, outcome = outcome)
#'
#' print(knn_brew)
#'

brew <- function(
  data,
  outcome,
  flavor = c('kneighbors', 'missRanger', 'softImpute'),
  bind_miss = FALSE
) {

  # Check outcome and transform to simple character value
  # TODO: come up with a more informative error message
  # for this if outcome is not in names of data

  outcome <- names(data) %>%
    tidyselect::vars_select(!!rlang::enquo(outcome)) %>%
    purrr::set_names(NULL)

  # check flavor input
  flavor <- flavor[1]
  check_flavor(flavor = flavor)

  # prepare data for brewing
  data %<>% brew_data(flavor = flavor, outcome = outcome, bind_miss)

  # Initiate the brew
  structure(
    .Data = list(
      data = data$impute,
      pars = list(),
      lims = get_par_bounds(data$impute, flavor),
      wort = NULL
    ),
    class = c('ipa_brew', paste(flavor, 'brew', sep = '_')),
    flavor = flavor,
    bind_miss = bind_miss,
    outcome = list(name = outcome, data = list(training = data$outcome)),
    verbose = 0,
    spiced = FALSE,
    mashed = FALSE,
    fermented = FALSE
  )

}

#' @rdname brew
#' @export
brew_nbrs <- function(data, outcome, bind_miss = FALSE){

  outcome <- names(data) %>%
    tidyselect::vars_select(!!rlang::enquo(outcome)) %>%
    purrr::set_names(NULL)

  brew(data = data, outcome = outcome,
    bind_miss = bind_miss, flavor = 'kneighbors')
}

#' @rdname brew
#' @export
brew_rngr <- function(data, outcome, bind_miss = FALSE){

  outcome <- names(data) %>%
    tidyselect::vars_select(!!rlang::enquo(outcome)) %>%
    purrr::set_names(NULL)

  brew(data = data, outcome = outcome,
    bind_miss = bind_miss, flavor = 'missRanger')
}

#' @rdname brew
#' @export
brew_soft <- function(data, outcome, bind_miss = FALSE){

  outcome <- names(data) %>%
    tidyselect::vars_select(!!rlang::enquo(outcome)) %>%
    purrr::set_names(NULL)

  brew(data = data, outcome = outcome,
    bind_miss = bind_miss, flavor = 'softImpute')

}

#' Print a brew
#'
#' Sometimes you need to see the brew.
#'
#' @param x an `ipa_brew` object.
#'
#' @param ... additional arguments (currently not used)
#'
#' @export

print.ipa_brew <- function(x, ...){

  flavor_expand <- switch(
    attr(x, 'flavor'),
    'kneighbors' = 'K-nearest-neighbors',
    'softImpute' = 'soft imputation',
    'missRanger' = 'imputation by random forests'
  )

  output <- glue::glue(
    "A \U1F37A to handle missing data using {flavor_expand}. \n\n",
    "Data used for imputation (outcomes are wittheld): \n\n\n"
  )

  cat(output)
  print(x$data)

}

#' @rdname print.ipa_brew
#' @export

is_brew <- function(x){
  inherits(x, 'ipa_brew')
}

#' Noisier brewing
#'
#' Sometimes you want to see text output to make sure
#'   your brew is brewing the way you want it to. Use this function
#'   to make your brew tell you what's going on at all subsequent
#'   stages.
#'
#' @param brew an `ipa_brew` object (see [brew]).
#' @param level The level of verbosity.
#'
#' @return a noisier `ipa_brew` object with
#'   an adjusted `verbose` attribute value.
#'
#' @export
#'

verbose_on <- function(brew, level){

  if(!is_brew(brew)) stop("the input is not an ipa_brew object")

  if(!level %in% c(1:2)) stop(
    'level should be 1 (a little verbose) or 2 (a lot verbose)'
  )

  attr(brew, 'verbose') <- level

  brew

}

#' @rdname verbose_on
#' @export
verbose_off <- function(brew){

  attr(brew, 'verbose') <- 0

  brew

}


#' @rdname verbose_on
#' @export
get_verbosity <- function(brew) attr(brew, 'verbose')

get_flavor      <- function(brew) attr(brew, 'flavor')
get_bind_miss   <- function(brew) attr(brew, 'bind_miss')
get_outcome     <- function(brew) attr(brew, 'outcome')
get_impute_args <- function(brew) attr(brew, 'impute_args')

brew_data <- function(data, outcome, flavor, bind_miss = FALSE){

  if(!tibble::is_tibble(data)){
    imp_data <- tibble::as_tibble(data)
  } else {
    imp_data <- data
  }

  check_brew_dat(imp_data, flavor = flavor)

  # outcomes should be removed prior to imputation
  # How are you going to impute missing values of X
  # if you need to know the outcome to impute X? If
  # you know the outcome, what are you predicting??

  if(outcome %in% names(imp_data)){

    imp_data[, outcome] <- NULL
    outcome_data <- data[, outcome, drop = FALSE]

  } else {

    outcome_data <- NULL

  }

  if(bind_miss) imp_data %<>% bind_miss_dat()

  list(
    impute = imp_data,
    outcome = outcome_data
  )

}

bind_miss_dat <- function(data, sep = '_', miss_chr = 'missing'){

  miss_cols <- data %>%
    purrr::map_dfc(.f = ~as.integer(is.na(.x))) %>%
    purrr::set_names(glue::glue("{names(.)}{sep}{miss_chr}"))

  data %>% dplyr::bind_cols(miss_cols)

}

check_brew_dat <- function(data, flavor){

  # Check for empty rows/cols
  all_rows_na <- apply(data, MARGIN = 1, function(x) all(is.na(x)))
  all_cols_na <- apply(data, MARGIN = 2, function(x) all(is.na(x)))

  if(any(all_rows_na)){
    stop("the following rows are missing data for all predictors: ",
      glue::glue_collapse(which(all_rows_na), sep = ', ', last = ' and '),
      call. = FALSE)
  }

  if(any(all_cols_na)){
    stop("the following columns are missing data for all values: ",
      glue::glue_collapse(
        names(data)[all_cols_na], sep = ', ', last = ' and '
      ),
      call. = FALSE)
  }

  var_types <- purrr::map_chr(data, class)

  if(!all(var_types %in% c('factor', 'integer', 'numeric'))){
    stop("Unsupported variable types in data. \n",
      "Valid types are factor, integer, and numeric.",
      call. = FALSE)
  }

  all_numerics <- all(var_types %in% c('integer', 'numeric'))

  if(!all_numerics && flavor == 'softImpute'){
    stop("Unsupported variable types in data\n",
      "For softImpute brews, all variables should be integer/numeric.",
      call. = FALSE)
  }

}


