
#' Elastic net imputation
#'
#' `glmnet` is used to impute missing values.
#'  For more details, see [glmnet][glmnet::glmnet()]
#'
#' @inheritParams impute_soft
#'
#' @param fit a list of lists of strings that contain formulas
#'   from previously fitted imputation models. This input variable
#'   is created using `data_ref` when `data_new = NULL`.
#'
#' @param df_min,df_max integer value designating the minimum
#'   and maximum degrees of freedom in penalized regression models.
#'
#' @param df_stp integer value indicating step size for model
#'   degrees of freedom between successive imputations.
#'
#' @export
#'

impute_net <- function(
  data_ref,
  data_new = NULL,
  fit = NULL,
  cols = dplyr::everything(),
  df_min = 1,
  df_max = 10,
  df_stp = 1,
  restore_data = TRUE,
  verbose = 1
){

  # keep_cols = columns to be imputed
  keep_cols <- names(data_ref) %>%
    tidyselect::vars_select(!!rlang::enquo(cols))

  if(length(keep_cols) == 1) stop("1 column was selected (",
    keep_cols, ") but 2+ are needed", call. = FALSE)

  # if data_ref is given and nothing else
  # --> create fits for data_ref, return fits + imputed data refs
  # if data_ref/data_new are given, but no fits
  # --> create fits for rbind(data_ref, data_new), return imputed data_news
  # if data_ref/data_new + fits are given,
  # --> same as above but use warm starts

  # convert data frames into data.table objects if needed
  if(!is.data.table(data_ref))
    DT_ref <- as.data.table(data_ref)[, ..keep_cols]
  else
    DT_ref <- data_ref[, ..keep_cols]

  # convert characters to factors
  # modifying in place rather than copying data
  # the code is less readable but more performant
  if(any(sapply(DT_ref, is.character))){
    chr_cols <- names(DT_ref)[sapply(DT_ref, is.character)]
    DT_ref[, (chr_cols) := lapply(.SD, as.factor), .SDcols = chr_cols]
  }

  # variable types should be...
  check_var_types(DT_ref, valid_types = c('numeric', 'integer', 'factor'))

  # fill in missing data with means/modes for first iteration
  fillers <- vector(mode = 'list', length = ncol(DT_ref))
  names(fillers) <- names(DT_ref)

  for(f in names(fillers)){
    fillers[[f]] <- switch (get_var_type(DT_ref[[f]]),
      'intg' = as.integer(round(mean(DT_ref[[f]], na.rm = TRUE))),
      'ctns' = mean(DT_ref[[f]], na.rm = TRUE),
      'catg' = mode_est(DT_ref[[f]]),
      'bnry' = mode_est(DT_ref[[f]])
    )
  }

  # initialize a null DT_new object in case there isn't any new data
  DT_new <- NULL
  data_new_supplied <- !is.null(data_new)

  # repeat the code above for the testing data if it exists.
  if(data_new_supplied){

    if(is.null(fit)) stop("fit is needed to impute new data.\nRun",
      " impute_net() with new_data=NULL to create fit", call.=FALSE)

    # convert data frames into data.table objects if needed
    if(!is.data.table(data_new))
      DT_new <- as.data.table(data_new)[, ..keep_cols]
    else
      DT_new <- data_new[, ..keep_cols]

    if(any(sapply(DT_new, is.character))){

      chr_cols <- names(DT_new)[sapply(DT_new, is.character)]
      DT_new[, (chr_cols) := lapply(.SD, as.factor), .SDcols = chr_cols]

    }

    # should have exactly the same names and types as reference data
    check_data_new_names(DT_ref, DT_new)
    check_data_new_types(DT_ref, DT_new)

  }

  DT <- DT_new %||% DT_ref

  # need to keep empty cells for check_missingness to run correctly
  miss_indx <- mindx(DT, drop_empty = FALSE)

  if(is_empty(miss_indx)){
    warning("There are no missing values to impute",
      call. = FALSE)
    return(data.table(
      impute = seq(n_impute), df = NA, fit = NA, imputed_values = list(NULL)
    ))
  }

  # check for missing rows/columns
  check_missingness(miss_indx, N = nrow(DT), P = ncol(DT),
    label = 'data', new_data = data_new_supplied)

  # drop empty cols from miss_indx
  miss_indx <- drop_empty(miss_indx)

  # don't need to fill in values where there are no missing values
  fillers <- fillers[names(miss_indx)]


  if(data_new_supplied){
    impute_net_fit(
      DT = DT,
      fit = fit,
      df_min = df_min,
      df_max = df_max,
      df_stp = df_stp,
      fillers = fillers,
      miss_indx = miss_indx,
      restore_data = restore_data
    )
  } else {
    impute_net_ref(
      DT = DT,
      df_min = df_min,
      df_max = df_max,
      df_stp = df_stp,
      fillers = fillers,
      miss_indx = miss_indx,
      restore_data = restore_data
    )
  }

}

impute_net_fit <- function(
  DT,
  fit,
  df_min = df_min,
  df_max = df_max,
  df_stp = df_stp,
  fillers,
  miss_indx,
  restore_data,
  niter = 3
) {

  df_sequence <- unique(round(seq(df_min, df_max, by = df_stp)))
  n_impute <- length(df_sequence)


  DT <- fill_na(DT, vals = fillers, miss_indx, make_copy = FALSE)
  fctr_info <- get_factor_info(DT)

  .DT <- as.matrix(one_hot(DT))

  # could this be optimized? make .miss_indx hold one hot factors
  # and make miss_indx hold continuous variables? no overlap?
  .miss_indx <- miss_indx

  for (f in names(fctr_info$keys)) {
    for (k in fctr_info$keys[[f]]) {
      .miss_indx[[k]] <- miss_indx[[f]]
    }
    .miss_indx[[f]] <- NULL
  }

  imputed_values <- vector(mode = 'list', length = n_impute)

  for(i in seq_along(imputed_values)) {
    imputed_values[[i]] <- .miss_indx
  }

  for(k in seq(niter)){

    for(i in seq_along(fit)){

      col <- .col <- names(fit)[i]
      vtype <- get_var_type(DT[[col]])

      if(col %in% fctr_info$cols){
        .col <- fctr_info$keys[[col]]
      }

      .xvars <- setdiff(colnames(.DT), .col)

      for(j in seq_along(imputed_values)){


        prd <- predict(fit[[col]],
          s = fit[[col]]$lambda_ipa[j],
          newx = .DT[miss_indx[[col]], .xvars],
          type = if(vtype %in% c('ctns', 'intg')) 'response' else 'class'
        )

        if(vtype %in% c('catg', 'bnry')){
          prd <- one_hot_chr(prd, fctr_info$keys[[col]])
        }

        if (k < niter) {

          .DT[miss_indx[[col]], .col] <- prd

        } else {

          if(vtype %in% c('catg', 'bnry')){
            for (h in seq_along(.col)) {
              imputed_values[[j]][[.col[h]]] <- prd[, h]
            }
          } else if (vtype == 'ctns') {
            imputed_values[[j]][[.col]] <- as.numeric(prd)
          } else if (vtype == 'intg') {
            imputed_values[[j]][[.col]] <- as.integer(round(prd))
          }

        }

      }

    }

  }

  if(restore_data){
    # converting the imputed columns back into the format given in data.
    # this should leave us with a list that can be directly plugged in.
    imputed_values <- purrr::map(
      .x = imputed_values,
      .f = restore_vectypes,
      data = DT,
      impute_indx = miss_indx,
      fctr_info = fctr_info
    )

  }

}



impute_net_ref <- function(
  DT,
  df_min,
  df_max,
  df_stp,
  fillers,
  miss_indx,
  restore_data
) {

  df_sequence <- unique(round(seq(df_min, df_max, by = df_stp)))
  n_impute <- length(df_sequence)

  DT <- fill_na(DT, vals = fillers, miss_indx, make_copy = FALSE)
  # names with . in front indicate one-hot encoded data
  # both needed - don't try to optimize
  .DT <- as.matrix(one_hot(DT))

  fctr_info <- get_factor_info(DT)

  imputed_values <- vector(mode = 'list', length = n_impute)
  fits <- vector(mode = 'list', length = length(miss_indx))
  names(fits) <- names(miss_indx)

  for(i in seq_along(imputed_values)) {
    imputed_values[[i]] <- miss_indx
  }

  #impute_formulas <- imputed_values

  for(impute_col in names(miss_indx)){

    family <- switch (get_var_type(DT[[impute_col]]),
      'intg' = 'gaussian',
      'ctns' = 'gaussian',
      'catg' = 'multinomial',
      'bnry' = 'binomial'
    )

    .outcome <- if(impute_col %in% fctr_info$cols){
      fctr_info$keys[[impute_col]]
    } else {
      impute_col
    }

    .predictors <- setdiff(colnames(.DT), .outcome)

    fit <- glmnet::glmnet(
      x = .DT[-miss_indx[[impute_col]], .predictors, drop = FALSE],
      y = .DT[-miss_indx[[impute_col]], .outcome, drop = FALSE],
      family = family,
      nlambda = 100,
      dfmax = min(df_max+10, length(.predictors))
    )

    # butchering
    # fit$call <- NULL
    # fit$dim <- NULL
    # fit$dev.ratio <- NULL
    # fit$nulldev <- NULL
    # fit$npasses <- NULL
    # fit$jerr <- NULL
    # fit$nobs <- NULL

    df_indx <- df_unique_indx(fit$df)
    df_vals <- fit$df[df_indx]
    df_keep <- between(df_vals, lower = df_min, upper = df_max)
    df_indx_subset <- df_indx[df_keep]

    lambda <- fit$lambda[df_indx_subset]

    if(length(lambda) < n_impute){

      ntimes <- n_impute - length(lambda)
      lambda <- c(lambda, rep(lambda[length(lambda)], ntimes))

    } else if(length(lambda) > n_impute){

      lambda <- lambda[1:n_impute]

    }

    fit$lambda_ipa <- lambda

    yh <- stats::predict(fit,
      newx = .DT[miss_indx[[impute_col]], .predictors],
      s = lambda,
      type = 'response')

    #cf <- stats::coef(fit, s = fit$lambda[df_indx])

    fits[[impute_col]] <- fit

    if(family == 'multinomial'){

      #cfs <- lapply(cf, deparse_net_coef)

      for(j in seq(n_impute)){
        for(k in seq_along(fctr_info$lvls[[impute_col]])){
          fctr_term <- fctr_info$keys[[impute_col]][k]
          imputed_values[[j]][[fctr_term]] <- yh[, k, j]
          #impute_formulas[[j]][[fctr_term]] <- cfs[[k]][[j]]
        }
      }

    } else {

      #cf <- deparse_net_coef(cf)

      if(family == 'binomial'){

        fctr_terms <- fctr_info$keys[[impute_col]]

        for(j in seq(n_impute)){

          imputed_values[[j]][[fctr_terms[1]]] <- 1 - yh[, j]
          imputed_values[[j]][[fctr_terms[2]]] <- yh[, j]

          # impute_formulas[[j]][[fctr_terms[1]]] <- paste("-1*(",cf[[j]],")")
          # impute_formulas[[j]][[fctr_terms[2]]] <- cf[[j]]

        }

      } else {

        for(j in seq(n_impute)){
          imputed_values[[j]][[impute_col]] <- yh[, j]
          #impute_formulas[[j]][[impute_col]] <- cf[[j]]
        }

      }

    }

  }

  if(restore_data){
    # converting the imputed columns back into the format given in data.
    # this should leave us with a list that can be directly plugged in.
    imputed_values <- purrr::map(
      .x = imputed_values,
      .f = restore_vectypes,
      data = DT,
      impute_indx = miss_indx,
      fctr_info = fctr_info
    )

  }

  data.table(
    impute = seq(n_impute),
    df = df_sequence,
    fit = list(fits),
    imputed_values = imputed_values
  )

}

deparse_net_coef <- function(net_prd){

  as.matrix(net_prd) %>%
    as.data.table(keep.rownames = 'id') %>%
    melt.data.table(
      id.vars = 'id',
      variable.name = 'impute',
      value.name = 'coef'
    ) %>%
    .[coef != 0] %>%
    split(f = .$impute) %>%
    lapply(FUN = function(dt){

      var_names <- dt$id
      var_coefs <- dt$coef

      intrcpt <- which(var_names == '(Intercept)')
      .intrcpt <- NULL

      if(!is_empty(intrcpt)){

        .intrcpt <- paste0(var_coefs[intrcpt])
        var_names <- var_names[-intrcpt]
        var_coefs <- var_coefs[-intrcpt]

        if(!is_empty(var_names)) .intrcpt <- paste0(.intrcpt, ' + ')

      }

      formula_terms <- NULL

      if(!is_empty(var_names)){
        formula_terms <- paste(var_names, "*", var_coefs)
        formula_terms <- paste(formula_terms, collapse = " + ")
      }

      paste0(.intrcpt, formula_terms)

    })

}

