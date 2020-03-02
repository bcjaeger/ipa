

#' New brew
#'
#' `brew()` is the first function in the `ipa` workflow .
#'
#' Brewing a great beer is not that different from imputing
#'   missing data. Once a brew is started, you can add spices
#'   (set primary parameters; see [spice]) and then mash the mixture
#'   (fitting imputation models; see [mash]). To finish the brew, add
#'   yeast (new data; see [ferment]), and then bottle it up (see
#'   [bottle]) as a `tibble` or `matrix`.
#'
#'   `brew()` includes an input variable called `flavor` that determines
#'     how data will be imputed.  `brew_nbrs()` and `brew_soft()` are
#'     convenience functions, e.g. `brew_nbrs()` is a
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
#'   impute missing data. Valid options are 'kneighbors' and 'softImpute'.
#'   These values should be input as characters (e.g., 'kneighbors').
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
#'
#' knn_brew <- brew_nbrs(data, outcome = outcome)
#' sft_brew <- brew_soft(data, outcome = outcome)
#'
#' print(knn_brew)
#'

brew <- function(
  data,
  outcome,
  flavor = c('kneighbors', 'softImpute'),
  bind_miss = FALSE
) {

  # Check outcome and transform to simple character value

  outcome <- names(data) %>%
    tidyselect::vars_select(!!rlang::enquo(outcome)) %>%
    purrr::set_names(NULL)

  # check flavor input
  flavor <- flavor[1]
  check_flavor(flavor = flavor)

  # convert to data.table
  setDT(data)

  check_var_types(data, c('numeric', 'integer', 'factor'))

  cols <- setdiff(names(data), outcome)
  check_data_ref(data[, ..cols])

  if (any(is.na(data[, ..outcome]))) stop(
    glue::glue("missing values in outcome columns ",
      "({list_things(outcome)}) are not allowed."),
    call. = FALSE
  )

  # outcomes should be removed prior to imputation
  # How are you going to impute missing values of X
  # if you need to know the outcome to impute X? If
  # you know the outcome, what are you predicting??

  if(outcome %in% names(data)){

    outcome_data <- data[, ..outcome]
    data[[outcome]] <- NULL

  } else {

    outcome_data <- NULL

  }

  # Initiate the brew
  structure(
    .Data = list(
      data = list(training = data),
      miss = list(training = mindx(data, drop_const = TRUE)),
      lims = get_par_bounds(data, flavor),
      pars = list(),
      wort = NULL
    ),
    class = c('ipa_brew', paste(flavor, 'brew', sep = '_')),
    flavor = flavor,
    bind_miss = bind_miss,
    outcome = list(name = outcome, data = list(training = outcome_data)),
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
brew_soft <- function(data, outcome, bind_miss = FALSE){

  outcome <- names(data) %>%
    tidyselect::vars_select(!!rlang::enquo(outcome)) %>%
    purrr::set_names(NULL)

  brew(data = data, outcome = outcome,
    bind_miss = bind_miss, flavor = 'softImpute')

}

#' Print a brew
#'
#' Sometimes you need to check the brew.
#'
#' @param x an `ipa_brew` object.
#'
#' @param ... additional arguments (currently not used)
#'
#' @export

print.ipa_brew <- function(x, ...){

  flavor_expand <- switch(
    attr(x, 'flavor'),
    'kneighbors' = 'k-nearest-neighbors',
    'softImpute' = 'soft imputation'
  )

  if(is_mashed(x)){

    cat(glue::glue(
      "A \U1F37A to handle missing data using {flavor_expand}. \n\n"
    ))
    print(tibble::as_tibble(x$wort))

  } else {

    cat(glue::glue(
      "A \U1F37A to handle missing data using {flavor_expand}. \n",
      "Data used for imputation (outcomes are wittheld): \n\n"
    ))
    if(get_bind_miss(x)){

      dplyr::bind_cols(x$data$training, x$miss$training) %>%
        tibble::as_tibble() %>%
        print()

    } else {
      print(tibble::as_tibble(x$data$training))
    }

  }

}


# helpers for working with brew attributes
get_flavor      <- function(brew) attr(brew, 'flavor')
get_bind_miss   <- function(brew) attr(brew, 'bind_miss')
get_outcome     <- function(brew) attr(brew, 'outcome')
get_composition <- function(brew) attr(brew, 'composition')







