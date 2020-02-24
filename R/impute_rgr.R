
impute_ranger <- function(
  data_ref,
  data_new = NULL,
  node_size,
  maxiter = NULL,
  num.trees = NULL,
  sample.fraction = NULL,
  donor_size = NULL,
  verbose = 0
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

  new_data_supplied <- !is.null(data_new)

  data <- if(new_data_supplied){
    dplyr::bind_rows(data_ref, data_new)
  } else {
    data_ref
  }

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

  if(new_data_supplied){

    fits <- purrr::map(
      .x = fits,
      .f = ~ .x[-c(1:nrow(data_ref)), , drop = FALSE]
    )

  }

  fits

}
