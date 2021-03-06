#' Spice a brew
#'
#' @description When an `ipa_brew` is spiced, tuning parameters
#'   are designated and saved in the `pars` component of the `brew`.
#'
#' @param brew an `ipa_brew` object.
#'
#' @param ... additional arguments for specific brew flavors.
#'
#' @param with a helper function for mashing brews.
#'   See [spicer_nbrs] and [spicer_soft])
#'
#' @return an `ipa_brew` object with values added to `pars`.
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
#' sft_brew <- brew(data, outcome = outcome, flavor = 'softImpute')
#'
#' # these two calls are equivalent
#' spicy_knn <- spice(knn_brew, with = spicer_nbrs(k_neighbors = c(3, 5)))
#' spicy_knn <- spice(knn_brew, k_neighbors = c(3, 5))
#'
#' # these two calls are equivalent
#' spicy_sft <- spice(sft_brew, with = spicer_soft(grid = TRUE))
#' spicy_sft <- spice(sft_brew, grid = TRUE)
#'
#'
spice <- function(brew, with = NULL, ...){

  UseMethod("spice")

}

#' @export
spice.kneighbors_brew <- function(brew, with = NULL, ...){

  check_brew(brew, expected_stage = 'spice')
  check_spicer(with, expected = 'spicer_nbrs')

  args <- with %||% check_dots(list(...),
    valid_args = c('k_neighbors', 'aggregate')) %>%
    # drop the spicer class
    unclass()

  args$k_neighbors <- args$k_neighbors %||% seq(5, 50, by = 5)
  args$aggregate <- args$aggregate %||% TRUE

  if (check_l1_warn(args$aggregate, label = 'aggregate'))
    args$aggregate <- args$aggregate[1]

  lims <- get_lims(brew)

  if (any(args$k_neighbors < lims$neighbors$min)) {

    warning(glue::glue(
      "neighbor values < {lims$neighbors$min} have been removed"),
      call. = FALSE)

    args$k_neighbors <-
      args$k_neighbors[args$k_neighbors >= lims$neighbors$min]

  }

  if (any(args$k_neighbors > lims$neighbors$max)) {

    warning(glue::glue("neighbor values > {lims$neighbors$max}",
    " (max no. of neighbors for the given data) have been removed"),
      call. = FALSE)

    args$k_neighbors <-
      args$k_neighbors[args$k_neighbors <= lims$neighbors$max]

  }

  if(purrr::is_empty(args$k_neighbors)){

    stop("all k_neighbor values were outside of the expected range",
      call. = FALSE)

  }


  brew$pars <- args

  if(get_verbosity(brew) > 0) message("Spices being used:\n",
    text_pillar(lhs = names(brew$pars), rhs = brew$pars,
      middle = 'has value(s)'))

  attr(brew, 'n_impute') <- length(args$k_neighbors)
  attr(brew, 'spiced')   <- TRUE

  brew

}

#' @export

spice.softImpute_brew <- function(brew, with = NULL, ...){

  check_brew(brew, expected_stage = 'spice')

  check_spicer(with, expected = 'spicer_soft')

  args <- with %||% check_dots(
    list(...), valid_args = c('rank_max_init','rank_max_ovrl',
      'rank_stp_size','lambda','grid')) %>%
    # drop the spicer class
    unclass()

  # add defaults if needed
  args$rank_max_init <- args$rank_max_init %||% 2
  args$rank_stp_size <- args$rank_stp_size %||% 1
  args$rank_max_ovrl <- args$rank_max_ovrl %||% min(dim(brew$data$training)-1)
  args$lambda <- args$lambda %||% seq(args$rank_max_ovrl*.6, 1, length.out=10)
  args$grid <- args$grid %||% FALSE

  check_int(args$rank_max_init, label = 'initial max rank (rank_max_init)')
  check_int(args$rank_stp_size, label = 'rank step size (rank_stp_size)')

  if(!is.null(args$rank_max_ovrl))
    check_int(args$rank_max_ovrl, label = 'overall max rank')

  check_bool(args$grid, label = 'grid')
  check_l1_warn(args$grid, label = 'grid')

  lims <- get_lims(brew)

  check_min_lax(args$rank_max_init,
    label = 'initial max rank (rank_max_init)',
    value = lims$rank_max_init$min)

  check_max_lax(args$rank_max_init,
    label = 'initial max rank (rank_max_init)',
    value = lims$rank_max_init$max)

  check_min_lax(args$rank_max_ovrl,
    label = 'overall max rank (rank_max_ovrl)',
    value = lims$rank_max_ovrl$min)

  check_max_lax(args$rank_max_ovrl,
    label = 'overall max rank (rank_max_ovrl)',
    value = lims$rank_max_ovrl$max)

  check_min_lax(args$rank_stp_size,
    label = 'rank step size (rank_stp_size)',
    value = lims$rank_stp_size$min
  )

  check_min_lax(args$lambda, label = 'lambda', value = 0)

  n_rank <- with(args, seq(rank_max_init, rank_max_ovrl, by = rank_stp_size))
  n_lambda <- length(args$lambda)

  brew$pars <- args

  if(get_verbosity(brew) > 0) message("Spices being used:\n",
    text_pillar(lhs = names(brew$pars), rhs = brew$pars,
      middle = 'has value(s)'))

  attr(brew, 'n_impute') <- if (args$grid) n_rank * n_lambda else n_lambda
  attr(brew, 'spiced')   <- TRUE

  brew

}


#' Soft spices
#' @inherit spicer_nbrs description return examples
#' @inheritParams impute_soft
#' @export
spicer_soft <- function(rank_max_init = 2L, rank_max_ovrl = NULL,
  rank_stp_size = 1L, lambda = NULL, grid = FALSE
){

  structure(
    .Data = list(
      rank_max_init = rank_max_init,
      rank_max_ovrl = rank_max_ovrl,
      rank_stp_size = rank_stp_size,
      lambda = lambda,
      grid = grid[1]
    ),
    class = 'spicer_soft'
  )
}

#' Neighbor's spices
#' @inherit masher_nbrs description
#' @inherit spice examples
#' @inheritParams impute_nbrs
#' @return a list with input values that can be passed directly into
#'   [spice], e.g `spice(brew, with = spicer_nbrs())` for a neighbors
#'   brew or `spice(brew, with = spicer_soft())` for a soft brew.
#' @export

spicer_nbrs <- function(
  k_neighbors = seq(10),
  aggregate = TRUE
){

  structure(
    .Data = list(k_neighbors = k_neighbors,
      aggregate = aggregate),
    class = 'spicer_nbrs'
  )

}


is_spicer <- function(x){
  inherits(x, paste("spicer", c('nbrs','soft'), sep = '_'))
}
