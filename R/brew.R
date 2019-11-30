

#' Brews for Imputation
#'
#' Brewing a delicious beer is not that different from imputing
#'   missing data. Once a brew is started, you can add spices
#'   (set tuning parameters; see [spice]) or just go right to mashing
#'   (fitting imputation models; see [mash]). To finish off the brew, add
#'   yeast (new data; see [ferment]), and then bottle it up (see [bottle])
#'   and share an `ipa_brew` with your friends!
#'
#' @param data a data frame with missing values.
#'
#' @param outcome column name(s) of outcomes in `data`.
#'   These values can be provided as symbols (e.g., outcome = c(a,b,c)
#'   for multiple outcomes or outcome = a for one outcome) or
#'   character values (e.g., outcome = c('a','b','c') for multiple outcomes
#'   or outcome = 'a' for a single outcome).
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
#' print(knn_brew)
#'

brew <- function(
  data,
  outcome,
  flavor = c('kneighbors', 'missRanger', 'softImpute')
) {

  # Check outcome and transform to simple character value
  # TODO: come up with a more informative error message
  # for this if outcome is not in names of data
  outcome <- names(data) %>%
    tidyselect::vars_select(!!rlang::enquo(outcome)) %>%
    purrr::set_names(NULL)

  # Check the flavor
  flavor <- flavor[1]
  good_flavors <- c('kneighbors','softImpute','missRanger')
  glue_flavors <- glue::glue_collapse(good_flavors, sep = ', ', last = ', or ')

  if( !(flavor %in% good_flavors) ){
    stop('flavor should be one of ', glue_flavors, call. = FALSE)
  }

  # prepare data for brewing
  data %<>% brew_data(flavor = flavor, outcome = outcome)

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
    outcome = list(name = outcome, data = list(training = data$outcome)),
    verbose = 0,
    spiced = FALSE,
    mashed = FALSE,
    fermented = FALSE
  )

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

get_verbosity <- function(brew){
  attr(brew, 'verbose')
}



brew_data <- function(data, outcome, flavor){

  if(!tibble::is_tibble(data)){
    imp_data <- tibble::as_tibble(data)
  } else {
    imp_data <- data
  }

  # outcomes should be removed prior to imputation
  # How are you going to impute missing values of X
  # if you need to know the outcome to impute X? If
  # you know the outcome, what are you predicting??
  imp_data[, outcome] <- NULL

  # Check for empty rows/cols
  all_rows_na <- apply(imp_data, MARGIN = 1, function(x) all(is.na(x)))
  all_cols_na <- apply(imp_data, MARGIN = 2, function(x) all(is.na(x)))

  if(any(all_rows_na)){
    stop("the following rows are missing data for all predictors: ",
      glue::glue_collapse(which(all_rows_na), sep = ', ', last = ' and '),
      call. = FALSE)
  }

  if(any(all_cols_na)){
    stop("the following columns are missing data for all values: ",
      glue::glue_collapse(
        names(imp_data)[all_cols_na], sep = ', ', last = ' and '
      ),
      call. = FALSE)
  }

  var_types <- purrr::map_chr(imp_data, class)

  if(!all(var_types %in% c('factor', 'integer', 'numeric'))){
    stop("Unsupported variable types in data. \n",
      "Valid types are factor, integer, and numeric.",
      call. = FALSE)
  }

  all_numerics <- all(var_types %in% c('integer', 'numeric'))

  if(!all_numerics && flavor[1] == 'softImpute'){
    stop("Unsupported variable types in data\n",
      "For softImpute brews, all variables should be integer/numeric.",
      call. = FALSE)
  }

  list(
    impute = imp_data,
    outcome = data[, outcome, drop = FALSE]
  )

}
