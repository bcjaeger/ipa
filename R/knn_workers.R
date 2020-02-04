
mode_est <- function(x){

  if (!is.character(x) & !is.factor(x))
    stop(
      "The data should be character or factor to compute the mode.",
      call. = FALSE
    )

  tab <- table(x)
  modes <- names(tab)[tab == max(tab)]
  sample(modes, size = 1)

}

nn_pred <- function(index, dat, k, random = FALSE){

  dat <- dat[index[1:k], ]
  dat <- getElement(dat, names(dat))
  dat <- dat[!is.na(dat)]

  if(random) return( dat[sample.int(length(dat), size = 1)] )

  if (is.factor(dat) | is.character(dat)) return(mode_est(dat))

  mean(dat)

}

nn_impute <- function(
  ref_data,
  new_data,
  nn_index,
  neighbors = 5,
  random = FALSE
) {

  # This function is used after finding nn_index,
  # nn_index contains nearest neighbors for every
  # column in ref and new data. Generally, this
  # only works when the ref_data is imputed because
  # it assumes no missing values are in ref_data.

  for(i in 1:ncol(new_data)) {

    # loop through each column,
    # find missing values in that column
    # replace the missing values with imputed ones

    na_index <- is.na(new_data[, i])

    if(any(na_index)){

      na_index <- which(na_index)

      new_data[na_index, i] <- apply(
        X = nn_index[, na_index, drop = FALSE],
        MARGIN = 2,
        FUN = nn_pred,
        dat = ref_data[, i, drop = FALSE],
        k = neighbors,
        random = random
      )

    }

  }

  new_data

}

knn_work <- function(
  ref_data,
  new_data = NULL,
  neighbor_sequence,
  neighbor_aggregate,
  nthread = getOption("gd_num_thread"),
  epsilon = 1e-08,
  verbose = TRUE
) {

  n_impute <- length(neighbor_sequence)

  if(length(neighbor_sequence) != length(neighbor_aggregate)) stop(
    "neighbors and aggr_neighbors should be the same length or length one.",
    call. = FALSE
  )

  new_data_supplied <- !is.null(new_data)

  new_data <- new_data %||% ref_data

  if(!tibble::is_tibble(new_data))
    new_data <- tibble::as_tibble(new_data)

  fit_args <- fits <- vector(mode = 'list', length = n_impute)

  for( i in seq(n_impute) ) {
    fits[[i]] <- new_data
    fit_args[[i]] <- list(
      neighbors = neighbor_sequence[i],
      aggregate = neighbor_aggregate[i]
    )
  }

  missing_rows <- !stats::complete.cases(new_data)

  if ( !any(missing_rows) ) return(fits)

  for ( i in seq(ncol(new_data)) ) {

    imp_var <- names(new_data)[i]

    missing_rows <- !stats::complete.cases(new_data[, imp_var])

    if ( any(missing_rows) ) {

      preds <- names(new_data)[-i]

      imp_data <- new_data[missing_rows, preds, drop = FALSE]

      ## do a better job of checking this:
      if ( all( is.na(imp_data) ) ) {

        warning(
          "All predictors are missing; cannot impute",
          call. = FALSE
        )

      } else {

        cols_na <- purrr::map_lgl(imp_data, ~all(is.na(.x))) %>%
          which() %>%
          names()

        if(!purrr::is_empty(cols_na)){
          preds <- setdiff(preds, cols_na)
          imp_data <- new_data[missing_rows, preds, drop = FALSE]
        }

        imp_var_complete <- if(new_data_supplied){
          !is.na(ref_data[[imp_var]])
        } else {
          !is.na(new_data[[imp_var]])
        }

        n_complete <- sum(imp_var_complete)

        n_gower <- min(
          n_complete,
          max(neighbor_sequence)
        )

        if(verbose)
          message(
            glue::glue(
              "Imputing {imp_var} (N = {n_complete})"
            )
          )


        y <- if(new_data_supplied){
          ref_data[imp_var_complete, preds, drop = FALSE]
        } else {
          new_data[imp_var_complete, preds, drop = FALSE]
        }

        nn_index <- suppressWarnings(
          gower::gower_topn(
            x = imp_data[, preds, drop = FALSE],
            y = y,
            n = n_gower,
            nthread = nthread,
            eps = epsilon
          )$index
        )

        dat <- if(new_data_supplied){
          ref_data[imp_var_complete, imp_var, drop = FALSE]
        } else {
          new_data[imp_var_complete, imp_var, drop = FALSE]
        }

        pred_vals <- purrr::map2(
          .x = neighbor_sequence,
          .y = neighbor_aggregate,
          .f = ~ apply(
            X = nn_index,
            MARGIN = 2,
            FUN = nn_pred,
            dat = dat,
            random = !(.y),
            k = .x
          )
        )

        for(j in 1:n_impute){
          fits[[j]][missing_rows, imp_var] <- pred_vals[[j]]
        }
      }
    }
  }

  tibble::tibble(impute = seq(n_impute), fit = fits, args = fit_args)

}
