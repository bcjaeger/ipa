
sip <- function(
  data_imputed,
  data_missing,
  data_complete,
  fun_ctns_error = yardstick::rsq_vec,
  fun_bnry_error = yardstick::kap_vec,
  fun_catg_error = yardstick::kap_vec
){

  output <- purrr::set_names(names(data_missing)) %>%
    purrr::map_dbl(
      .f = sip_col,
      data_imputed   = data_imputed,
      data_missing   = data_missing,
      data_complete  = data_complete,
      fun_ctns_error = fun_ctns_error,
      fun_bnry_error = fun_bnry_error,
      fun_catg_error = fun_catg_error
    ) %>%
    tibble::enframe(name = 'variable', value = 'score') %>%
    dplyr::transmute(
      variable,
      perc_miss = map_dbl(variable, ~mean(is.na(data_missing[[.x]]))),
      score
    )

  output

}

sip_col <- function(col,
  data_missing,
  data_complete,
  data_imputed,
  fun_ctns_error,
  fun_bnry_error,
  fun_catg_error){

  missing_indx <- is.na(data_missing[[col]])

  if(!any(missing_indx)) return(NA_real_)

  truth <- data_complete[[col]][missing_indx]
  estimate <- data_imputed[[col]][missing_indx]

  switch(get_var_type(data_complete[[col]]),
    'ctns' = fun_ctns_error(estimate = estimate, truth = truth),
    'bnry' = fun_bnry_error(estimate = estimate, truth = truth),
    'catg' = fun_catg_error(estimate = estimate, truth = truth))

}
