



# TODO: identify numeric 0/1 columns and throw error
# stop(all binary variables should be coded as factors)

init_output <- function(data, max_iter){

  out = tibble::tibble(
    variable = names(data),
    miss_perc = purrr::map_dbl(
      .x = variable,
      .f = ~mean(is.na(data[[.x]]))
    )
  )

  out$nrounds = NA_real_

  for(i in seq(max_iter))
    out[[paste('error', i, sep = '_')]] <- NA_real_

  out$converged = 0
  out$model <- list(NULL)
  out


}

xgb_impute <- function(
  data,
  mdl = NULL, # named list of xgb models or named list of files to read in
  max_iter = 5,
  max_iter_cv = 1,
  nrounds = 50,
  nfolds = 5,
  eta = 0.3,
  gamma = 0.5,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1,
  num_parallel_tree = 1,
  verbose = 1,
  trn_prop = 2/3,
  eval_ctns = 'rmse',
  eval_bnry = 'logloss',
  eval_catg = 'mlogloss'
){

  data <- data %>%
    dplyr::mutate_if(is.character, as.factor)

  xgb_data <- data.table::as.data.table(data) %>%
    mltools::one_hot(dropCols = TRUE, dropUnusedLevels = FALSE) %>%
    as.matrix()

  no_mdl <- is.null(mdl)

  if(no_mdl){

    params = list(
      eta = eta,
      gamma = gamma,
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      num_parallel_tree = num_parallel_tree
    )

    xgb_fit_and_impute(
      data = data,
      xgb_data = xgb_data,
      max_iter = max_iter,
      max_iter_cv = max_iter_cv,
      nrounds = nrounds,
      nfolds = nfolds,
      params = params,
      verbose = verbose,
      trn_prop = trn_prop,
      eval_ctns = eval_ctns,
      eval_bnry = eval_bnry,
      eval_catg = eval_catg
    )

  } else {

    xgb_impute_with_fit(
      data = data,
      mdl = mdl,
      xgb_data = xgb_data,
      max_iter = max_iter,
      nrounds = nrounds,
      params = params,
      verbose = verbose,
      trn_prop = trn_prop,
      eval_ctns = eval_ctns,
      eval_bnry = eval_bnry,
      eval_catg = eval_catg
    )

  }


}


xgb_fit_and_impute <- function(
  data,
  xgb_data,
  max_iter,
  max_iter_cv,
  nrounds,
  nfolds,
  params,
  verbose,
  trn_prop,
  eval_ctns,
  eval_bnry,
  eval_catg
){

  output <- init_output(data = data, max_iter = max_iter)

  # set up tuning parameters for mixed variable types
  ctns_prms <- params
  bnry_prms <- params
  catg_prms <- params

  ctns_prms$objective <- 'reg:squarederror'
  bnry_prms$objective <- 'reg:logistic'
  catg_prms$objective <- 'multi:softmax'

  ctns_prms$eval_metric <- eval_ctns
  bnry_prms$eval_metric <- eval_bnry
  catg_prms$eval_metric <- eval_catg

  cols_to_impute <- output$variable[output$miss_perc > 0]

  for(iter in seq(max_iter)){

    message("beginning iteration no. ", iter)

    cols_converged <- output$variable[output$converged > 0]

    for(impute_col in setdiff(cols_to_impute, cols_converged)){

      var_type <- get_var_type(data[[impute_col]])

      xgb_info <- get_xgb_info(data, impute_col, var_type)

      xgb_params <- switch(var_type, 'ctns' = ctns_prms,
        'bnry' = bnry_prms, 'catg' = catg_prms)

      if(var_type == 'catg') xgb_params$num_class <- xgb_info$num_classes

      # identify rows where impute_col needs to be imputed
      imp_indx <- which(is.na(data[, impute_col]))

      # identify rows where we can fit a model to impute_col
      xgb_indx <- setdiff(seq(nrow(data)), imp_indx)

      # This is why impute_cols is created above.
      # xgb_features should not include any of the one-hot
      # columns that contain outcome values.
      xgb_features <- setdiff(colnames(xgb_data), xgb_info$label_name)

      # make an xgb.Dmatrix object for training/testing sets
      xgb_Dtrn <- xgboost::xgb.DMatrix(
        data  = xgb_data[xgb_indx, xgb_features],
        label = xgb_info$label_value[xgb_indx])

      variable_indx <- which(output$variable == impute_col)

      nrounds_var <- output$nrounds[variable_indx]

      if(iter <= max_iter_cv) nrounds_var <- NA

      xgb_fit <- .xgb_fit(
        data      = xgb_Dtrn,
        params    = xgb_params,
        nfolds    = nfolds,
        nrounds_max = nrounds,
        nrounds_var = nrounds_var,
        verbose   = verbose,
        print_every_n = round(nrounds / 10, digits = 0)
      )

      # update the model history data

      output$nrounds[variable_indx]      <- xgb_fit$niter
      output[['model']][variable_indx]   <- list(xgb_fit)

      # need to identify which error column to update
      error_col <- paste('error', iter, sep = '_')
      # the updated error value is filled in using the score values
      # from the xgb_fit evaluation log. We are taking the score value
      # from the best iteration of the xgb_fit model, i.e., the last
      output[[error_col]][variable_indx] <-
        xgb_fit$evaluation_log[[2]][xgb_fit$niter]

      if(iter > 1){

        prev_error_col <- paste('error', iter-1, sep = '_')

        error_ratio <- magrittr::divide_by(
          output[[prev_error_col]][variable_indx],
          output[[error_col]][variable_indx]
        )

        if(error_ratio < 1.005 && error_ratio > 0.9995)
          output$converged[variable_indx] <- iter

      }

      # predict missing values of impute_col
      xgb_prd <- predict(xgb_fit, newdata = xgb_data[imp_indx, xgb_features])

      if(var_type != 'ctns'){

        if(var_type == 'bnry') xgb_prd <- round(xgb_prd, digits = 0)

        xgb_prd <- one_hot(xgb_prd, length(xgb_info$label_name))

      }

      xgb_data[imp_indx, xgb_info$label_name] <- xgb_prd

    }

  }

  factor_levels <- data %>%
    dplyr::select_if(is.factor) %>%
    purrr::map(levels)

  imputed_data <- xgb_data %>%
    tibble::as_tibble() %>%
    one_cold(factor_levels)

  list(imputed = imputed_data,
    log = output)

}


.xgb_fit <- function(
  data,
  params,
  nfolds,
  nrounds_max,
  nrounds_var,
  verbose,
  print_every_n
) {

  if(is.na(nrounds_var)){

    cv_fit <- xgboost::xgb.cv(
      data = data,
      params = params,
      nfold = nfolds,
      nrounds = nrounds_max,
      verbose = verbose,
      print_every_n = print_every_n,
      early_stopping_rounds = 20
    )

    xgboost::xgboost(
      data = data,
      params = params,
      nrounds = cv_fit$best_iteration,
      verbose = 0
    )

  } else {

    xgboost::xgboost(
      data = data,
      params = params,
      nrounds = nrounds_var,
      verbose = verbose
    )

  }

}

one_cold <- function(
  data,
  factor_levels
) {

  if(purrr::is_empty(factor_levels)) return(data)

  old_names <- new_names <- names(data)

  factor_names <- factor_levels %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = value) %>%
    dplyr::mutate(factor_name = paste(name, value, sep = "_")) %>%
    dplyr::select(factor_name, name) %>%
    tibble::deframe()

  new_names <- unique(dplyr::recode(old_names, !!!factor_names))

  for(i in seq_along(factor_levels)){

    .factor <- names(factor_levels)[i]
    .levels <- factor_levels[[i]]
    .names <- paste(.factor, .levels, sep = '_')

    new_col <- data[, .names] %>%
      apply(1, which.max) %>%
      as.numeric() %>%
      factor(levels = 1:length(.names), labels = .levels)

    data[[.factor]] <- new_col
    data[, .names] <- NULL

  }

  data[, new_names]

}


one_hot <- function(x, ncats){

  x <- x + 1

  mat <- matrix(0, ncol = ncats, nrow = length(x))

  for(i in seq(ncats)) mat[x==i, i] <- 1

  mat

}

get_var_type <- function(x){

  if(is.numeric(x)){
    return('ctns')
  }

  if(is.factor(x)){

    n_lvls <- length(levels(x))

    if(n_lvls == 2){
      return('bnry')
    } else {
      return('catg')
    }

  }

  stop("incompatible variable type <", class(x)[1], "> in data")

}

get_xgb_info <- function(data, col, var_type){

  switch(
    var_type,
    "ctns" = get_xgb_info_ctns(data, col),
    "bnry" = get_xgb_info_bnry(data, col),
    "catg" = get_xgb_info_catg(data, col)
  )

}

get_xgb_info_ctns <- function(data, col){

  # if impute_col is continuous, then this isn't needed.
  # For code to be consistent, copy some values

  list(
    label_name = col,
    label_value = data[[col]],
    num_classes = 0
  )

}

# there will be multiple columns in xgb_data representing col
# if col is categorical. mltools::one_hot creates these columns
# by taking <variable>_<levels>. Apply the same pattern so that the
# impute columns in one_hot data are correctly identified

# create a label value that xgboost is compatible with.
# (must be an integer ranging from 0 to n_levels -1)

get_xgb_info_bnry <- function(data, col){

  list(
    label_name = paste(col, levels(data[[col]]), sep = '_'),
    label_value = as.integer(data[[col]]) - 1L,
    num_classes = 2L
  )

}

get_xgb_info_catg <- function(data, col){

  list(
    label_name  = paste(col, levels(data[[col]]), sep = '_'),
    label_value = as.integer(data[[col]]) - 1L,
    num_classes = max(as.integer(data[[col]]), na.rm = TRUE)
  )

}

