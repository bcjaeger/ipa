missRanger_work <- function(
  data,
  verbose,
  maxiter = NULL,
  num.trees = NULL,
  sample.fraction = NULL,
  node_size,
  donor_size = NULL
){

  n_impute <- length(node_size)

  # If unspecified, assume donor size is 0 and
  # give it the same length as node_size so that
  # they are easily passed into map2
  donor_size = donor_size %||% rep(0, n_impute)

  # Fill in default imputation parameters wherever the
  # user-input does not make an explicit specification.
  # Sync with the ranger mash function
  dflt_pars <- masher_rngr()

  .dots <- list(
    data = data,
    verbose = verbose,
    maxiter = maxiter %||% dflt_pars$maxiter,
    num.trees = num.trees %||% dflt_pars$num.trees,
    sample.fraction = sample.fraction %||% dflt_pars$sample.fraction
  )

  fits <- purrr::map2(
    .x = node_size,
    .y = donor_size,
    .f = ~ {
      .dots$pmm.k <- .y
      .dots$min.node.size <- .x
      do.call(missRanger::missRanger, args = .dots)
    }
  )

  fit_args <- vector(mode = 'list', length = n_impute)

  for(i in seq_along(fit_args)) fit_args[[i]] <- list(
    min_node_sizes = node_size[i],
    pmm_donor_sizes = donor_size[i],
    maxiter = .dots$maxiter,
    num.trees = .dots$num.trees,
    sample.fraction = .dots$sample.fraction
  )

  tibble::tibble(impute = seq(n_impute), fit = fits, args = fit_args)

}
