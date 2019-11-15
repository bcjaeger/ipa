#' Spice a brew
#'
#' @description When an `ipa_brew` is spiced, tuning parameters
#'   are designated and saved in the `pars` component of the `brew`.
#'
#' @param brew an `ipa_brew` object.
#'
#' @param ... additional arguments for specific brew flavors.
#'
#' @param with the output of a helper function for spicing brews.
#'   The helper functions are [spicer_nbrs], [spicer_soft], and
#'   [spicer_rngr].
#'
#' @return a spiced `ipa_brew`, with imputation
#'    parameters just how you like them.
#'
#' @export
#'
#' @examples
#'
#' data <- data.frame(
#'   x1 = 1:10,
#'   x2 = 10:1,
#'   x3 = rnorm(10),
#'   outcome = 11 + runif(10)
#' )
#'
#' data[1:2, 1:2] = NA
#'
#' knn_brew <- brew(data, outcome = outcome, flavor = 'kneighbors')
#' spicy_knn <- spice(knn_brew, neighbors = c(3, 5), aggr_neighbors = TRUE)
#'
#' sft_brew <- brew(data, outcome = outcome, flavor = 'softImpute')
#' spicy_sft <- spice(sft_brew, n_impute = 2)
#'


spice <- function(brew, with = NULL, ...){

  UseMethod("spice")

}

simple_spice <- function(brew){

  check_brew(brew, expected_stage = 'spice')

  spice(brew)

}

#' @export

spice.kneighbors_brew <- function(brew, with = NULL, ...){

  check_brew(brew, expected_stage = 'spice')
  check_spicer(with, expected = 'spicer_nbrs')

  args <- with %||% check_dots(
    list(...),
    valid_args = c(
      'neighbors',
      'aggr_neighbors'
    )
  )

  nb_seq <- args$neighbors %||% seq(min(brew$lims$neighbors$max, 10))

  if( any(nb_seq < brew$lims$neighbors$min) ) {
    stop(glue::glue("all neighbor sequence values ",
      "must be >= {brew$lims$neighbors$min}"),
      call. = FALSE
    )
  }

  if( any(nb_seq > brew$lims$neighbors$max) ) {
    stop(glue::glue("all neighbor sequence values ",
      "must be <= {brew$lims$neighbors$max}"),
      call. = FALSE
    )
  }

  brew$pars <- list(
    n_impute = length(nb_seq),
    nbrs = nb_seq,
    aggr = args$aggr_neighbors %||% TRUE
  )

  attr(brew, 'spiced') <- TRUE

  brew


}

#' @export

spice.missRanger_brew <- function(brew, with = NULL, ...){

  check_brew(brew, expected_stage = 'spice')
  check_spicer(with, expected = 'spicer_rngr')

  args <- with %||% check_dots(
    list(...),
    valid_args = c(
      'min_node_sizes',
      'pmm_donor_sizes'
    )
  )

  ns_seq <- args$min_node_sizes

  if( any(ns_seq < brew$lims$node_size$min) ) {
    stop(glue::glue("all node size values ",
      "must be >= {brew$lims$node_size$min}"),
      call. = FALSE
    )
  }

  if( any(ns_seq > brew$lims$node_size$max) ) {
    stop(glue::glue("all node size values ",
      "must be <= {brew$lims$node_size$max}"),
      call. = FALSE
    )
  }

  pm_seq <- args$pmm_donor_sizes %||% rep(0, length(ns_seq))

  if(length(pm_seq) == 1)
    pm_seq <- rep(pm_seq, length(ns_seq))

  if(length(pm_seq) != length(ns_seq))
    stop(
      'predictive mean matching donor sequence should be ',
      'length 1 or the same length as node size sequence.',
      call. = FALSE
    )

  brew$pars <- list(
    n_impute = length(ns_seq),
    node_size = ns_seq,
    donor_size = pm_seq
  )

  attr(brew, 'spiced') <- TRUE

  brew


}

#' @export

spice.softImpute_brew <- function(brew, with = NULL, ...){

  check_brew(brew, expected_stage = 'spice')
  check_spicer(with, expected = 'spicer_soft')

  args <- with %||% check_dots(
    list(...),
    valid_args = c(
      'n_impute',
      'step_size'
    )
  )

  n_impute <- args$n_impute %||% min(10, brew$lims$rank$max)
  step_size <- args$step_size %||% 1L

  check_pos_int(n_impute, label = 'number of imputations')
  check_pos_int(step_size, label = 'step size')

  check_step_size(
    step_size = step_size,
    n_impute =  n_impute,
    max_rank =  brew$lims$rank$max
  )

  brew$pars <- list(
    min_rank = brew$lims$rank$min,
    max_rank = brew$lims$rank$max,
    n_impute = n_impute,
    step_size = step_size
  )

  attr(brew, 'spiced') <- TRUE

  brew

}


#' Soft spices
#'
#' It can be a little overwhelming to remember which sets of
#' parameters go with each `ipa_brew` flavor, so just add a dash
#' from a handy `spicer` function and get on with your `brew`.
#'
#' @param n_impute An integer indicating the number of imputed
#'    datasets to create.
#'
#' @param step_size An integer indicating the number by which to increase
#'    the maximum rank of the softImpute solution after each iteration.
#'
#' @return a list with parameter values that can be passed
#'   directly into `missRanger_brew` objects via [spice].
#'
#' @concept spices
#'
#' @export
#'
spicer_soft <- function(n_impute=NULL, step_size = 1L){
  check_pos_int(step_size, label = 'step size')
  structure(
    .Data = list(n_impute = n_impute, step_size = step_size),
    class = 'spicer_soft'
  )
}

#' Ranger's spices
#'
#' It can be a little overwhelming to remember which sets of
#' parameters go with each `ipa_brew` flavor, so just add a dash
#' from a handy `spicer` function and get on with your `brew`.
#'
#' @param min_node_sizes Minimal node size of a leaf in a decision tree.
#'   Increasing the minimal node size reduces the chance of overfitting.
#'
#' @param pmm_donor_sizes Number of candidate non-missing values to
#'   sample from in the predictive mean matching step.
#'   Set `pmm_donor_sizes = 0` to skip this step.
#'
#' @return a list with parameter values that can be passed
#'   directly into `missRanger_brew` objects via [spice].
#'
#' @concept spices
#'
#' @export
#'

spicer_rngr <- function(min_node_sizes = NULL, pmm_donor_sizes = 0L) {
  structure(
    .Data = list(min_node_sizes = min_node_sizes,
      pmm_donor_sizes = pmm_donor_sizes),
    class = 'spicer_rngr'
  )
}

#' Neighbor's spices
#'
#' It can be a little overwhelming to remember which sets of
#' parameters go with each `ipa_brew` flavor, so just add a dash
#' from a handy `spicer` function and get on with your `brew`.
#'
#' @param neighbors the number of neighbors used for imputation.
#'    One imputed dataset is created for each value given. For
#'    example, to make two imputed datasets using 1 and 2
#'    nearest neighbors, one would write `neighbors = c(1,2)`
#'
#' @param aggr_neighbors (`TRUE` / `FALSE`) whether neighbor's
#'   values should be aggregated or sampled. If `TRUE`, then
#'   the mean/mode of neighboring values will be used. Otherwise,
#'   one neighbor's value will be sampled at random.
#'
#' @return a list with parameter values that can be passed
#'   directly into `kneighbors_brew` objects via [spice].
#'
#' @concept spices
#'
#' @export
#'
#'

spicer_nbrs <- function(neighbors=NULL, aggr_neighbors = TRUE){
  stopifnot(is.logical(aggr_neighbors))
  structure(
    .Data = list(neighbors = neighbors, aggr_neighbors = aggr_neighbors),
    class = 'spicer_nbrs'
  )
}

#' @rdname spice
#' @export

is_spiced <- function(brew){
  attr(brew, 'spiced')
}
