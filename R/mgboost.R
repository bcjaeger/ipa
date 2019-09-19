

#' (midy) Cross-validation for xgboosters
#'
#' @description This function is a wrapper for the
#'   [xgboost::xgb.cv()] function that automates some
#'   adjustments that should be applied when using `midy` data.
#'   For example, the `subsample` tuning parameter is automatically
#'   adjusted based on the number of imputed datasets for `midy` data.
#'   Additionally, the folds argument is adjusted such that
#'   observations in `midy` data belonging to the same ID are
#'   allocated to the same data-fold.
#'
#' @details To ensure valid data-folds, the `folds` argument must be
#'   supplied. If `nfolds` is supplied instead, an error will occur.
#'
#' @inheritParams xgboost::xgb.cv
#'
#' @return a modified version of the output from
#'   [xgboost::xgb.cv()]. Specifically,
#'   an additional item, `compute_time` is included
#'   in the output list.
#'
#' @export

# .dots <- list(
#   data = train$knn_lst,
#   label = label$train,
#   params = params,
#   verbose = 1,
#   print_every_n = 50,
#   early_stopping_rounds = 100,
#   folds = folds,
#   nrounds = 5000
# )
# miss_strat = 'stack'
# n_impute = length(nb_seq)


mgb_cv <- function(..., miss_strat, n_impute = NULL){

  check_miss_strat(miss_strat = miss_strat)
  if(miss_strat == 'si') n_impute <- 1


  .dots <- list(...) %>%
    check_dots(
      valid_args = c(
        "params",
        "data",
        "nrounds",
        "label",
        "missing",
        "prediction",
        "showsd",
        "metrics",
        "obj",
        "feval",
        "folds",
        "verbose",
        "print_every_n",
        "early_stopping_rounds",
        "maximize",
        "callbacks"
      )
    )

  if( !('data' %in% names(.dots)) ) {
    stop('data needs to be specified', call. = FALSE)
  }

  if( !('params' %in% names(.dots)) ){
    stop('params needs to be specified', call. = FALSE)
  }

  if ( !('folds' %in% names(.dots)) ) {
    stop("folds must be specified", call. = FALSE)
  }

  if('nfold' %in% names(.dots)){
    stop(
      "To ensure valid comparisons, please specify only the folds argument",
      call. = FALSE
    )
  }

  if(miss_strat == 'stacked'){

    .dots %<>% check_xgb_stack_args(n_impute = n_impute)

  }

  if(miss_strat == 'mi'){

    data_list <- prep_data_list(data=.dots$data, n_impute = n_impute)
    .dots$data <- NULL
    output <- mgb_cv_mi(data_list, args=.dots)

  } else {

    start <- Sys.time()
    output <- do.call(xgb.cv, args = .dots)
    stop <- Sys.time()

    output %<>% mgb_cmp_time(start = start, stop = stop, args = .dots)

  }

  output

}

mgb_cmp_time <- function(object, start, stop, args){

  if('early_stopping_rounds' %in% names(args)){
    divby <- object$best_iteration + args$early_stopping_rounds
  } else {
    divby <- args$nrounds
  }

  object$compute_time <- list(
    overall = stop-start,
    by_iter = as.numeric(stop-start) / divby
  )

  object

}

mgb_cv_mi <- function(data_list, args){

  map(
    .x = data_list,
    .f = function(data){

      args$data <- data

      start <- Sys.time()
      output = do.call(xgb.cv, args)
      stop <- Sys.time()

      output %<>% mgb_cmp_time(start = start, stop = stop, args = args)
      output

    }
  )

}



#' (midy) training for xgboosters
#'
#' @description This function is a wrapper for the
#'   [xgboost::xgb.train()] function that automates some
#'   adjustments that should be applied when using `midy` data.
#'   Specifically, the `subsample` tuning parameter is automatically
#'   adjusted based on the number of imputed datasets for `midy` data.
#'
#' @details When a list of multiply imputed data are supplied,
#'   each dataset will be used to train one xgboost model, separately.
#'   When a `midy` dataset is supplied, only one xgboost model is
#'   trained, and the stacked data are subsampled such that each
#'   boosting step utilizes N / n_impute, where N is the number of
#'   rows in the stacked `midy` data.
#'
#' @inheritParams xgboost::xgb.train
#'
#' @return a modified version of the output from
#'   [xgboost::xgb.cv()]. Specifically,
#'   an additional item, `compute_time` is included
#'   in the output list.
#'
#' @export
#'

# .dots <- list(
#   data = tmp$training[[2]],
#   params = params,
#   verbose = 1,
#   print_every_n = 50,
#   folds = folds,
#   nrounds = map(tmp$cv_obj[[2]],'best_iteration')
# )

mgb_train <- function(..., miss_strat, n_impute = NULL){

  check_miss_strat(miss_strat = miss_strat)
  if(miss_strat == 'si') n_impute <- 1

  .dots <- list(...) %>%
    check_dots(
      valid_args = c(
        "data",
        "label",
        "missing",
        "weight",
        "params",
        "nrounds",
        "verbose",
        "print_every_n",
        "early_stopping_rounds",
        "maximize",
        "save_period",
        "save_name",
        "xgb_model",
        "callbacks"
      )
    )

  orig_label <- .dots$label
  n_obs <- nrow(.dots$data) / n_impute

  if( !('data' %in% names(.dots)) ){
    stop('data needs to be specified', call. = FALSE)
  }

  if( !('params' %in% names(.dots)) ){
    stop('params needs to be specified', call. = FALSE)
  }

  if(miss_strat == 'stacked'){

    .dots %<>% check_xgb_stack_args(n_impute = n_impute)

  }


  if(miss_strat == 'mi'){

    data_list <- prep_data_list(data=.dots$data, n_impute = n_impute)

    if(length(.dots$nrounds) == n_impute){
      nround_list <- .dots$nrounds
    } else if(length(.dots$nrounds) == 1){
      nround_list <- rep(.dots$nrounds, n_impute)
    } else {
      stop(
        "nround should be length 1 or n_impute",
        call. = FALSE
      )
    }

    .dots$data <- NULL
    .dots$nrounds <- NULL

    output <- list(
      fit = mgb_train_mi(data_list, nround_list, args=.dots)
    )

    output$train_predictions <- map2(
      .x = output$fit,
      .y = data_list,
      .f = ~ predict(
        object = .x,
        newdata = .y,
        outputmargin = TRUE
      )
    ) %>%
      reduce(`+`) %>%
      divide_by(n_impute) %>%
      as.numeric()

    output$train_label <- orig_label


  } else {

    start <- Sys.time()
    output <- list(fit = do.call(xgboost, args = .dots))
    stop <- Sys.time()

    output$fit %<>% mgb_cmp_time(start = start, stop = stop, args = .dots)

    output$train_predictions <- predict(
      object = output$fit,
      newdata = .dots$data,
      outputmargin = TRUE
    ) %>%
      pool_preds(
        n_obs = n_obs,
        n_impute = n_impute,
        miss_strat = miss_strat
      ) %>%
      as.numeric()

    output$train_label <- orig_label


  }

  output$n_impute <- n_impute
  output$miss_strat <- miss_strat

  class(output) <- glue("mgb_booster")

  return(output)

}

mgb_train_mi <- function(data_list, nround_list, args){

  map2(
    .x = data_list,
    .y = nround_list,
    .f = function(data, nrounds){
      args$data <- data
      args$nrounds <- nrounds
      start <- Sys.time()
      output <- do.call(xgboost, args)
      stop <- Sys.time()
      output %<>% mgb_cmp_time(start = start, stop = stop, args = args)
      output
    }
  )

}

mgb_predict_mi <- function(object_list, newdata_list, args){

  newdata_is_dmat <- class(newdata_list)[1] == "xgb.DMatrix"

  if(newdata_is_dmat){

    args$newdata <- newdata_list

    map(
      .x = object_list,
      .f = function(object){
        args$object <- object
        #start <- Sys.time()
        output <- do.call(predict, args)
        #stop <- Sys.time()
        #output$compute_time <- stop-start
        output
      }
    )

  } else {

    right_length <- length(object_list) == length(newdata_list)

    if(!right_length) stop(
      glue("For mgboost objects based on multiply imputed data, \\
        newdata should be (1) a dataset or (2) a list of n_impute \\
        datasets, where n_impute = the number of imputed datasets."),
      call. = FALSE
    )

    map2(
      .x = object_list,
      .y = newdata_list,
      .f = function(object, newdata){
        args$object <- object
        args$newdata <- newdata
        #start <- Sys.time()
        output <- do.call(predict, args)
        #stop <- Sys.time()
        #output$compute_time <- stop-start
        output
      }
    )

  }



}

#' midy gradient boosting predictions
#'
#' @description Compute predictions from midy gradient boosting
#'   decision tree ensembles.
#'
#' @param object an object of class `xgb.Booster`.
#' @param newdata an object that inherits one of the following
#'   classes: `si_dmat`, `mi_dmat`, or `midy_dmat`.
#' @param outputmargin `TRUE` / `FALSE`. If `TRUE`, the predictions are
#'   returned as an untransformed sum of predictions from the boosting
#'   ensemble. For example, setting `outputmargin`=`TRUE` for logistic
#'   regression would result in predictions for log-odds instead of
#'   probabilities.
#' @param ntreelimit The number of trees or boosting iterations
#'   to be used when forming a sum of predicted values (see Details).
#'   If unspecified, all trees in the ensemble will be used.
#' @param predleaf `TRUE` / `FALSE`. When `predleaf` = `TRUE`, the output
#'   is a matrix object with the number of columns corresponding
#'   to the number of trees.
#' @param predcontrib `TRUE` / `FALSE`. Whether to return
#'   contributions to individual predictions (see Details).
#' @param approxcontrib `TRUE` / `FALSE`. Whether to use a fast
#'   approximation for feature contributions (see Details).
#' @param predinteraction `TRUE` / `FALSE`. Whether to return
#'   contributions of feature interactions to individual
#'   predictions (see Details).
#' @param reshape `TRUE` / `FALSE`. Whether to reshape the
#'   vector of predictions to a matrix form when there are
#'   several prediction outputs per case. This option has no
#'   effect when either of `predleaf`, `predcontrib`, or
#'   `predinteraction` flags is TRUE.
#'
#' @details
#' Note that `ntreelimit` is not necessarily equal to the
#' number of boosting iterations and it is not necessarily equal
#' to the number of trees in a model. E.g., in a random forest-like
#' model, `ntreelimit` would limit the number of trees.
#' But for multiclass classification, while there are multiple trees
#' per iteration, `ntreelimit` limits the number of boosting iterations.
#'
#' Setting `predcontrib = TRUE` allows to calculate contributions
#' of each feature to individual predictions. For "gblinear" booster,
#' feature contributions are simply linear terms (feature_beta *
#' feature_value). For "gbtree" booster, feature contributions are SHAP
#' values (Lundberg 2017) that sum to the difference between the expected
#' output of the model and the current prediction (where the hessian
#' weights are used to compute the expectations). Setting
#' `approxcontrib = TRUE` approximates these values following the idea
#' explained in \url{http://blog.datadive.net/interpreting-random-forests/}.
#'
#' With `predinteraction = TRUE`, SHAP values of contributions of
#' interaction of each pair of features are computed. Note that this
#' operation might be rather expensive in terms of compute and memory.
#' Since it quadratically depends on the number of features, it is
#' recommended to perform selection of the most important features first.
#' See below about the format of the returned results.
#'
#' @return
#' For regression or binary classification, it returns a vector of
#' length `nrow(newdata)`. For multiclass classification, either a
#' `num_class * nrow(newdata)` vector or a `(nrow(newdata), num_class)`
#'  dimension matrix is returned, depending on the `reshape` value.
#'
#' When `predleaf = TRUE`, the output is a matrix object with the
#' number of columns corresponding to the number of trees.
#'
#' When `predcontrib = TRUE` and it is not a multiclass setting,
#' the output is a matrix object with `num_features + 1` columns.
#' The last "+ 1" column in a matrix corresponds to bias.
#' For a multiclass case, a list of `num_class` elements is returned,
#' where each element is such a matrix. The contribution values are on
#' the scale of untransformed margin (e.g., for binary classification
#' would mean that the contributions are log-odds deviations from bias).
#'
#' When `predinteraction = TRUE` and it is not a multiclass setting,
#' the output is a 3d array with dimensions
#' `c(nrow, num_features + 1, num_features + 1)`. The off-diagonal
#' (in the last two dimensions) elements represent different features
#' interaction contributions. The array is symmetric WRT the last
#' two dimensions. The "+ 1" columns corresponds to bias.
#' Summing this array along the last dimension should produce
#' practically the same result as predict with `predcontrib = TRUE`.
#' For a multiclass case, a list of `num_class` elements is returned,
#' where each element is such an array.
#'
#' @seealso
#' [mgb_train()],
#' [mgb_cv()],
#' and
#' [mgb_surv_prob()]
#'
#' @references
#'
#' Scott M. Lundberg, Su-In Lee, "A Unified Approach to Interpreting
#' Model Predictions", NIPS Proceedings 2017,
#' \url{https://arxiv.org/abs/1705.07874}
#'
#' Scott M. Lundberg, Su-In Lee, "Consistent feature attribution for
#' tree ensembles", \url{https://arxiv.org/abs/1706.06060}
#'
#' @note This function is a wrapper for `predict.xgb.Booster`,
#' an un-exported function from the `xgboost` package. The inputs
#' and details described here are copied from the documentation in
#' `predict.xgb.Booster`.

#' @export
mgb_predict <- function(
  object,
  newdata,
  outputmargin = FALSE,
  ntreelimit = NULL,
  predleaf = FALSE,
  predcontrib = FALSE,
  approxcontrib = FALSE,
  predinteraction = FALSE,
  reshape = FALSE
){

  .dots <- list(
    outputmargin = outputmargin,
    ntreelimit = ntreelimit,
    predleaf = predleaf,
    predcontrib = predcontrib,
    approxcontrib = approxcontrib,
    predinteraction = predinteraction,
    reshape = reshape
  )

  miss_strat <- get_miss_strat(newdata)
  n_impute <- get_n_impute(newdata)

  if( miss_strat == 'mi' ){

    output <- mgb_predict_mi(
      object_list = object,
      newdata_list = newdata,
      args = .dots
    ) %>%
      pool_preds(
        n_obs = nrow(newdata) / n_impute,
        n_impute = n_impute,
        miss_strat = miss_strat
      )

  } else {

    .dots$object <- object
    .dots$newdata <- newdata

    output <- do.call(
      what = predict,
      args = .dots
    )

    if( miss_strat == 'stacked' ) {

      output  %<>%
        pool_preds(
          n_obs = nrow(newdata) / n_impute,
          n_impute = n_impute,
          miss_strat = miss_strat
        )

    }


  }

  output

}

#' Baseline hazard function
#'
#' @description This function is a wrapper for the
#'   [gbm::basehaz.gbm()]. The function
#'   computes the Breslow estimator of the baseline
#'   hazard function for a proportional hazard
#'   regression model.
#'
#' @param label A numeric vector with time-to-event values, where
#'   censored observations have negative times and uncensored
#'   observations have positive times (see [label_for_survival()]).
#' @param predictions The predicted values of the regression model
#'   on the log hazard scale.
#' @param eval_times Values at which the baseline hazard will
#'   be evaluated.
#' @param smooth If `TRUE` `mgb_bhaz` will smooth the estimated
#'   baseline hazard using Friedman's super smoother
#'   [stats::supsmu()].
#' @param cumulative If `TRUE` the cumulative survival function
#'   will be computed.
#' @export
#'
mgb_bhaz <- function(
  mgb_booster,
  eval_times = NULL,
  smooth = FALSE,
  cumulative = TRUE
){

  if(any(mgb_booster$train_label == 0))
    stop(
      "Survival times (i.e. label) must be non-zero",
      call. = FALSE
    )

  basehaz.gbm(
    t = get_time(mgb_booster$train_label),
    delta = get_status(mgb_booster$train_label),
    f.x = mgb_booster$train_predictions,
    t.eval = eval_times,
    smooth = smooth,
    cumulative = cumulative
  )

}

#' Predicted values from `midy` boosters.
#'
#' @description Computing survival probabilities with gradient
#'   boosted decision tree ensembles requires predictions on
#'   the log-hazard scale, a vector of estimates of the
#'   baseline hazard function at specified times, and the
#'   set of times
#'
#' @param predictions a numeric vector of predicted values
#' @param base_haz a numeric vector with baseline hazard estimates
#'   (see [mgb_bhaz()])
#' @param eval_times a numeric vector with evaluation time values.
#'
#' @note it is critical that `outputmargin `be set to
#'   `FALSE` when predictions are drawn from the `xgboost`
#'   ensemble (see [mgb_predict()]). If it is set
#'   to `TRUE`, the predicted survival
#'   probabilities will not be valid.
#'
#' @export

mgb_surv_prob <- function(predictions, base_haz, eval_times){

  if(length(base_haz) != length(eval_times)){
    stop(
      "base_haz should be the same length as eval_times",
      call. = FALSE
    )
  }

  prb <- matrix(
    data = 0,
    nrow=length(predictions),
    ncol=length(eval_times)
  )

  for(i in 1:length(base_haz)){
    prb[,i] <- exp(-exp(predictions) * (base_haz[i]))
  }

  prb

}

pool_preds <- function(
  preds,
  n_obs,
  n_impute,
  miss_strat
) {

  if (miss_strat == 'stacked') {

    if(length(preds) > n_obs){
      grp = rep(1:n_obs, each = n_impute)
      output <- tapply(preds, grp, mean)
    } else {
      output <- preds
    }


  } else if (miss_strat == 'mi') {

    output <- reduce(preds, `+`) %>%
      divide_by(n_impute)

  } else {

    output <- preds

  }

  output

}


predict_mgb_surv <- function(
  object,
  newdata,
  newdata_is_midy,
  newdata_midy_ids,
  times,
  ...
){

  class(object) <- 'xgb.Booster'

  base_hazard <- basehaz.gbm(
    t = pluck(attr(object, 'outcome'), 1),
    delta = pluck(attr(object, 'outcome'), 2),
    f.x = attr(object, 'predictions'),
    t.eval = times,
    smooth = TRUE,
    cumulative = TRUE
  )

  predictions <- predict(object, newdata = newdata)

  if(newdata_is_midy){

    predictions <- newdata_midy_ids %>%
      mutate(pred = predictions) %>%
      group_by(._midy.ID_.) %>%
      summarise_all(mean) %>%
      pull(pred)

  }

  survival_probs <- matrix(
    data = 0,
    nrow = length(predictions),
    ncol = length(times)
  )

  for (i in seq_along(times)) {
    survival_probs[, i] <- exp(-exp(predictions) * (base_hazard[i]))

    probs_too_high <- survival_probs[, i] > 1

    if(any(probs_too_high)){
      survival_probs[which(probs_too_high), i] <- 1
    }

  }

  survival_probs

}

predict_mgb_regr <- function(
  object,
  newdata,
  newdata_is_midy,
  newdata_midy_ids,
  ...
){

  class(object) <- 'xgb.Booster'

  predictions <- predict(object, newdata = newdata, ...)

  if(newdata_is_midy){

    predictions <- newdata_midy_ids %>%
      mutate(pred = predictions) %>%
      group_by(._midy.ID_.) %>%
      summarise_all(mean) %>%
      pull(pred)

  }

  predictions

}

predict_mgb_clsf <- function(
  object,
  newdata,
  newdata_is_midy,
  newdata_midy_ids,
  ...
){

  class(object) <- 'xgb.Booster'

  predictions <- predict(object, newdata = newdata, ...)

  if(newdata_is_midy){

    predictions <- newdata_midy_ids %>%
      mutate(pred = predictions) %>%
      group_by(._midy.ID_.) %>%
      summarise_all(mean) %>%
      pull(pred)

  }

  predictions

}

