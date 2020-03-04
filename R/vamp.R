

vamp_mdl <- function(train_imputed, test_imputed, fun_model, ...){

  check_data_new_names(
    data_ref = train_imputed, label_ref = 'imputed training data',
    data_new = test_imputed,  label_new = 'imputed testing data'
  )

  check_data_new_types(
    data_ref = train_imputed, label_ref = 'imputed training data',
    data_new = test_imputed,  label_new = 'imputed testing data'
  )

  purrr::map2(
    train_imputed,
    test_imputed,
    fun_model,
    ...
  )

}

vamp_vars <- function(
  data_imputed,
  data_missing,
  data_complete,
  fun_ctns_error = yardstick::rsq_vec,
  fun_intg_error = yardstick::rsq_vec,
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

  output <- vector(mode = 'list', length = ncol(data_imputed))
  names(output) <- names(data_imputed)

  for(i in seq_along(output)){

    missing_indx <- is.na(data_missing[[i]])

    if(!any(missing_indx)){

      output[[i]] <- NA_real_

    } else {

      truth <- data_complete[[i]][missing_indx]
      estimate <- data_imputed[[i]][missing_indx]

      output[[i]] <- switch(get_var_type(data_complete[[i]]),
        'ctns' = fun_ctns_error(estimate = estimate, truth = truth),
        'intg' = fun_intg_error(estimate = estimate, truth = truth),
        'bnry' = fun_bnry_error(estimate = estimate, truth = truth),
        'catg' = fun_catg_error(estimate = estimate, truth = truth))
    }

  }

  tibble::enframe(output, name = 'variable', value = 'score') %>%
    dplyr::transmute(
      variable = factor(variable, levels = names(data_imputed)),
      type = purrr::map_chr(variable, ~get_var_type(data_imputed[[.x]])),
      score = as.numeric(score)
    ) %>%
    dplyr::arrange(variable) %>%
    dplyr::mutate(variable = as.character(variable))

}
