

#' Score imputations based on accuracy of downstream models
#'
#' @description Imputation of missing data is generally completed
#'   in order to fit downstream models that require complete input data.
#'   For supervised learning analyses, a key goal is to develop models
#'   with optimal accuracy, so analysts will likely want to use
#'   whatever imputation strategy provides the most accurate downstream
#'   model. `scrimp_mdl` facilitates this comparison.
#'
#' @param train_imputed an imputed data frame with training data.
#'
#' @param test_imputed an imputed data frame with testing data.
#'
#' @inheritParams brew
#'
#' @param .fun a function with at least three inputs: `.trn` `.tst`,
#'   and `outcome`. `scrimp_mdl()` will call your function as follows:
#'   `.fun(.trn = train_imputed, .tst = test_imputed, outcome = outcome, ...)`,
#'   where `...` is filled in by `.fun_args`. Generally, `.fun` should
#'
#'   1. develop a prediction model using `.trn`
#'   2. create predicted values for observations in `.tst`
#'   3. evaluates the predictions using a summary measure (e.g., R-squared,
#'      area underneath the ROC curve, Brier score, etc.).
#'
#'   See example below where a function using random forests is applied.
#'
#' @param .fun_args a named list with additional arguments for `.fun`.
#'
#' @return If you supply your own function using `.fun`, `scrimp_mdl`
#'  will return the output of `.fun`.  If `.fun` is left unspecified,
#'  a named list is returned with components
#'
#'  - `model_cv`: a model tuned by cross-validation and fitted to the
#'     training data
#'
#'  - `preds_cv`: the model's predicted values for internal testing data
#'
#'  - `preds_ex`: the model's predicted values for external testing data
#'
#'  - `score_ex`: a numeric value indicating external prediction accuracy
#'
#'  The list's contents will vary depending on `.fun_args`. By
#'  default, when `fun` is unspecified, `.fun_args` is governed
#'  by the [net_args()] function, which includes inputs of `keep_mdl`
#'  and `keep_prd`. The default value for `keep_mdl` is `FALSE` while
#'  that of `keep_prd` is `TRUE`, so users who want to receive a list
#'  including `model` should write `.fun_args = net_args(keep_mdl = TRUE)`
#'  when calling `scrimp_mdl`.
#'
#' @export
#'
#' @examples
#'
#' data("diabetes")
#' trn <- diabetes$missing[1:200, ]
#' tst <- diabetes$missing[-c(1:200), ]
#'
#' imputes <- brew_soft(trn, outcome = diabetes) %>%
#'   mash(with = masher_soft(si_maxit = 1000)) %>%
#'   stir() %>%
#'   ferment(data_new = tst) %>%
#'   bottle() %>%
#'   .[['wort']] %>%
#'   .[5, list(training, testing)]
#'
#' # use the default glmnet logistic regression model
#' dflt_output <- scrimp_mdl(
#'   train_imputed = imputes$training[[1]],
#'   test_imputed  = imputes$testing[[1]],
#'   outcome = diabetes)
#'
#' # use default glmnet and include fitted model in list output
#' include_mdls <- scrimp_mdl(
#'   train_imputed = imputes$training[[1]],
#'   test_imputed  = imputes$testing[[1]],
#'   outcome = diabetes,
#'   .fun_args = net_args(keep_mdl = TRUE))
#'
#' \dontrun{
#' # write your own function:
#' # note the function inputs can be ordered however you like, but the
#' # names of the inputs **must** include be .trn, .tst, and outcome
#' rngr_fun <- function(outcome, .trn, .tst, num_trees){
#'
#'   # make a model formula
#'   formula <- as.formula(paste(outcome, '~ .'))
#'   # fit a random forest with ranger (probability = TRUE -> predicted probs)
#'   mdl <- ranger::ranger(formula = formula, data = .trn,
#'     probability = TRUE, num.trees = num_trees)
#'   # prediction from ranger returns matrix with two columns, we need the 2nd.
#'   prd <- predict(mdl, data = .tst)$predictions[ , 2, drop = TRUE]
#'   # compute model AUC
#'   yardstick::roc_auc_vec(.tst[[outcome]], prd)
#'
#' }
#'
#' scrimp_mdl(
#'   train_imputed = imputes$training[[1]],
#'   test_imputed  = imputes$testing[[1]],
#'   outcome = 'diabetes',
#'   .fun = rngr_fun,
#'   .fun_args = list(num_trees = 100)
#' )#'
#' }


scrimp_mdl <- function(
  train_imputed,
  test_imputed,
  outcome,
  .fun = NULL,
  .fun_args = NULL
){

  if(missing(outcome)) stop("outcome must be specified", call. = FALSE)

  outcome <- names(train_imputed) %>%
    tidyselect::vars_select(!!rlang::enquo(outcome)) %>%
    purrr::set_names(NULL)

  check_data_new_names(
    data_ref = train_imputed, label_ref = 'imputed training data',
    data_new = test_imputed,  label_new = 'imputed testing data'
  )

  check_data_new_types(
    data_ref = train_imputed, label_ref = 'imputed training data',
    data_new = test_imputed,  label_new = 'imputed testing data'
  )

  user_supplied_fun <- !is.null(.fun)
  empty_args <- is.null(.fun_args)

  if(!user_supplied_fun && empty_args) .fun_args <- net_args()

  if(length(outcome) == 1){

    .fun <- .fun %||% switch (get_var_type(train_imputed[[outcome]]),
      'ctns' = net_ctns,
      'intg' = net_intg,
      'bnry' = net_bnry,
      'catg' = net_catg,
    )

  } else {

    .fun <- .fun %||% net_surv

  }

  args <- list(
    .trn    = train_imputed,
    .tst    = test_imputed,
    outcome = outcome
  )

  if(!user_supplied_fun) args$.dots <- .fun_args

  if(user_supplied_fun && !purrr::is_empty(.fun_args)){

    for(i in seq_along(.fun_args)){
      args[[names(.fun_args)[i]]] <- .fun_args[[i]]
    }

  }

  do.call(.fun, args)

}

guess_surv_names <- function(mtx){

  ux <- apply(mtx, 2, function(x) length(unique(x)))
  if(ux[1] == ux[2]) stop("unable to determine which column contains",
    "'status' outcomes and which column contains 'time' outcomes",
    call. = FALSE)
  # the column with less unique values is probably the status column
  out <- vector(mode='character', length = 2)
  out[which.min(ux)] <- 'status'
  out[which.max(ux)] <- 'time'
  out

}


#' glmnet arguments helper function
#'
#'
#' @param cmplx Value(s) of the penalty parameter lambda at which
#'   predictions are required. Default is the value s = "lambda.1se" stored
#'   on the CV object. Alternatively s = "lambda.min" can be used. If s
#'   is numeric, it is taken as the value(s) of lambda to be used.
#'   (For historical reasons the glmnet authors use the symbol 's' to
#'   reference this parameter)
#'
#' @param keep_mdl a logical value. If `TRUE`, then the output of
#'   [scrimp_mdl] will include the `glmnet` model object. Default is `FALSE`
#'
#' @param keep_prd a logical value. If `TRUE`, then the output of
#'   [scrimp_mdl] will include predictions from the `glmnet` model object.
#'   Default is `TRUE`
#' @param alpha the elastic net mixing parameter
#' @param weights observation weights. Default is 1 for each observation
#' @param foldid an optional vector of values between 1 and nfold
#'   identifying what fold each observation is in. If supplied,
#'   nfold can be missing.
#' @param nfolds number of folds - default is 10. Although nfolds can be
#'   as large as the sample size (leave-one-out CV), it is not recommended
#'   for large datasets. Smallest value allowable is nfolds=3
#'
#' @return a named list with arguments that should be passed into the
#'  `.fun_args` argument of [scrimp_mdl].
#'
#' @export
#'
#' @examples
#'
#' net_args()
#'
#' net_args(cmplx = 'lambda.min')
#'
net_args <- function(
  alpha = 1/2,
  cmplx = 'lambda.1se',
  weights = NULL,
  foldid = NULL,
  nfolds = 10,
  keep_mdl = FALSE,
  keep_prd = TRUE
) {

  list(
    alpha    = alpha,
    cmplx    = cmplx,
    weights  = weights,
    foldid   = foldid,
    nfolds   = if(is.null(foldid)) nfolds else max(foldid),
    keep_mdl = keep_mdl,
    keep_prd = keep_prd
  )

}

net_eval <- function(
  .trn,
  .tst,
  outcome,
  family,
  eval_fun,
  .dots
) {

  x_var <- setdiff(names(.trn), outcome)
  x_trn <- as.matrix(one_hot(dplyr::select(.trn, tidyselect::all_of(x_var))))
  x_tst <- as.matrix(one_hot(dplyr::select(.tst, tidyselect::all_of(x_var))))

  if(family != 'cox'){

    keep_cv_prd <- TRUE
    y_trn <- dplyr::pull(.trn, tidyselect::all_of(outcome))
    y_tst <- dplyr::pull(.tst, tidyselect::all_of(outcome))

  } else {

    keep_cv_prd <- FALSE
    cv_prd <- NULL

    y_trn <- as.matrix(dplyr::select(.trn, tidyselect::all_of(outcome)))
    y_tst <- as.matrix(dplyr::select(.tst, tidyselect::all_of(outcome)))

    if(!('time' %in% colnames(y_tst)) | !('status' %in% colnames(y_trn))){
      warning("if family = 'cox', outcomes should be named 'time' and",
        "'status'.\n scrimp_mdls() will try to guess which column is which",
        "based on unique values.\n Rename your outcome columns as 'time'",
        "and 'status' to avoid this warning.", call. = FALSE)
      colnames(y_trn) <- guess_surv_names(y_trn)
      colnames(y_tst) <- guess_surv_names(y_tst)
    }

  }

  mdl <- glmnet::cv.glmnet(
    x = x_trn,  y = y_trn,
    family  = family,
    alpha   = .dots$alpha,
    weights = .dots$weights,
    foldid  = .dots$foldid,
    nfolds  = .dots$nfolds,
    keep    = keep_cv_prd
  )

  prd <- stats::predict(mdl, newx = x_tst, s = .dots$cmplx, type = 'response')

  if(keep_cv_prd){

    cv_prd <- if(family == 'multinomial'){
      mdl$fit.preval[, , which(mdl$lambda == mdl[[.dots$cmplx]])]
    } else {
      mdl$fit.preval[, which(mdl$lambda == mdl[[.dots$cmplx]])]
    }

    # cv_prd <- switch (family,
    #   'multinomial' = multi_prob(cv_prd),
    #   'binomial' = binom_prob(cv_prd),
    #   'gaussian' = cv_prd
    # )

  }

  estimate <- if(family == 'multinomial'){
    matrix(prd, nrow = nrow(.tst), ncol = length(levels(.trn[[outcome]])))
  } else {
    as.numeric(prd)
  }

  truth <- switch (family,
    'binomial' = factor(y_tst, levels = levels(.trn[[outcome]])),
    'multinomial' = factor(y_tst, levels = levels(.trn[[outcome]])),
    'gaussian' = as.numeric(y_tst),
    'cox' = y_tst)

  output <- list(
    model_cv = if(.dots$keep_mdl) mdl else NULL,
    preds_cv = if(keep_cv_prd) cv_prd else NULL,
    preds_ex = if(.dots$keep_prd) prd else NULL,
    score_ex = eval_fun(truth = truth, estimate = estimate)
  )

  purrr::discard(output, is.null)

}

multi_prob <- function(prd){

  prd <- exp(prd)

  divby <- matrix(
    data = rep(rowSums(prd), each = ncol(prd)),
    nrow = nrow(prd),
    byrow = TRUE
  )

  prd / divby

}

binom_prob <- function(prd) exp(prd) / (1+exp(prd))

# wrapper function to make glmnet::Cindex consistent with yardstick functions
cnc_index <- function(truth, estimate){
  glmnet::Cindex(pred = estimate, y=truth)
}

net_surv <- function(.trn, .tst, outcome, .dots) net_eval(
  .trn, .tst, outcome, 'cox', cnc_index, .dots)

net_bnry <- function(.trn, .tst, outcome, .dots) net_eval(
  .trn, .tst, outcome, 'binomial', yardstick::roc_auc_vec, .dots)

net_catg <- function(.trn, .tst, outcome, .dots) net_eval(
  .trn, .tst, outcome, 'multinomial', yardstick::roc_auc_vec, .dots)

net_ctns <- function(.trn, .tst, outcome, .dots) net_eval(
  .trn, .tst, outcome, 'gaussian', yardstick::rsq_vec, .dots)

net_intg <- function(.trn, .tst, outcome, .dots) net_eval(
  .trn, .tst, outcome, 'gaussian', yardstick::rsq_vec, .dots)


#' Score imputations for specific variables
#'
#' @description If you want to evaluate how accurately an imputation
#'   procedure fills in missing values, `scrimp_vars` can help. Generally,
#'   `scrimp_vars` only applies to artificial situations where you
#'   ampute your data (i.e., make missing values), then impute it.
#'   For a more general imputation validation procedure, see [scrimp_mdl].
#'
#' @param data_imputed an imputed data frame.
#' @param data_missing the unimputed data frame.
#' @param data_complete a data frame containing the 'true' values
#'   that were 'missing'.
#' @param miss_indx an object returned from the [mindx] function applied to `data_missing`.
#' @param fun_ctns_error a function that will evaluate errors for
#'   continuous variables. Continuous variables have type `double`.
#'   Default is to use R-squared (see [yardstick::rsq()]).
#' @param fun_intg_error a function that will evaluate errors for
#'   integer valued variables. Default is to use R-squared
#'   (see [yardstick::rsq()]).
#' @param fun_bnry_error a function that will evaluate errors for
#'   binary variables (i.e., factors with 2 levels). Default
#'   is to use kappa agreement (see [yardstick::kap()]).
#' @param fun_catg_error a function that will evaluate errors for
#'   categorical variables (i.e., factors with >2 levels). Default
#'   is to use kappa agreement (see [yardstick::kap()]).
#'
#' @return a [tibble::tibble()] with columns `variable`, `type`,
#'   and `score`. The `score` column comprises output from the
#'   `error` functions.
#'
#' @note Kappa agreement is a similar to measuring classification accuracy,
#'   but is normalized by the accuracy that would be expected by chance
#'   alone and is very useful when one or more classes have large frequency
#'   distributions.
#'
#' @export
#'
#' @examples
#'
#' df_complete <- data.frame(a = 1:10, b = 1:10, c = 1:10, d=1:10,
#'   fctr = letters[c(1,1,1,1,1,2,2,2,2,2)])
#'
#' df_miss = df_complete
#' df_miss[1:3, 1] <- NA
#' df_miss[2:4, 2] <- NA
#' df_miss[3:5, 3] <- NA
#' df_miss[4:6, 5] <- NA
#'
#'
#' imputes <- list(a=1:3, b=2:4, c=3:5, fctr = factor(c('a','a','b')))
#'
#' df_imputed <- fill_na(df_miss, vals = imputes)
#'
#' scored <- scrimp_vars(df_imputed, df_miss, df_complete)

scrimp_vars <- function(
  data_imputed,
  data_missing,
  data_complete,
  miss_indx = NULL,
  fun_ctns_error = yardstick::rsq_trad_vec,
  fun_intg_error = yardstick::rsq_trad_vec,
  fun_bnry_error = yardstick::kap_vec,
  fun_catg_error = yardstick::kap_vec
){

  check_data_new_names(
    data_ref = data_imputed, data_new = data_missing,
    label_ref = 'imputed data', label_new = 'missing data'
  )

  check_data_new_names(
    data_ref = data_imputed, data_new = data_complete,
    label_ref = 'imputed data', label_new = 'complete data'
  )

  check_data_new_types(
    data_ref = data_imputed, data_new = data_missing,
    label_ref = 'imputed data', label_new = 'missing data'
  )

  check_data_new_types(
    data_ref = data_imputed, data_new = data_complete,
    label_ref = 'imputed data', label_new = 'complete data'
  )

  miss_indx <- miss_indx %||% mindx(data_missing)
  output <- vector(mode = 'list', length = length(miss_indx))
  names(output) <- names(miss_indx)

  for(i in names(output)){

    if(!any(miss_indx[[i]])){

      output[[i]] <- NA_real_

    } else {

      output[[i]] <- switch(
        get_var_type(data_complete[[i]]),
        'ctns' = fun_ctns_error(
          estimate = data_imputed[[i]][miss_indx[[i]]],
          truth    = data_complete[[i]][miss_indx[[i]]]
        ),
        'intg' = fun_intg_error(
          estimate = data_imputed[[i]][miss_indx[[i]]],
          truth    = data_complete[[i]][miss_indx[[i]]]
        ),
        'bnry' = fun_bnry_error(
          estimate = data_imputed[[i]][miss_indx[[i]]],
          truth    = data_complete[[i]][miss_indx[[i]]]
        ),
        'catg' = fun_catg_error(
          estimate = data_imputed[[i]][miss_indx[[i]]],
          truth    = data_complete[[i]][miss_indx[[i]]]
        )
      )
    }

  }

  output <- as.data.table(output) %>%
    melt(measure.vars = 1:length(output), value.name = 'score') %>%
    .[,variable := factor(variable, levels = names(data_imputed))] %>%
    .[,type := sapply(variable, function(x) get_var_type(data_imputed[[x]]))] %>%
    .[,score := as.numeric(score)] %>%
    set(i = which(.$score == -Inf), j = 'score', value = NA_real_) %>%
    .[order(variable)] %>%
    .[, variable := as.character(variable)] %>%
    setcolorder(neworder = c('variable', 'type', 'score'))

  output

}
