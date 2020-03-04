
sip <- function(
  brew,
  from = 'training',
  data_complete,
  fun_ctns_error = yardstick::rsq_vec,
  fun_intg_error = yardstick::rsq_vec,
  fun_bnry_error = yardstick::kap_vec,
  fun_catg_error = yardstick::kap_vec
) {

  # make sure the brew has been bottled
  check_brew(brew, expected_stage = 'sip')

  # this should either be training or testing,
  # but it is nice to let users write it without quotes
  # if they want to.
  .col <- names(brew$wort) %>%
    tidyselect::vars_select(!!rlang::enquo(from)) %>%
    purrr::set_names(NULL) %>%
    .[1]

  # if we are scoring training data, then we need to look at
  # the brew's training data. Same thing goes for scoring testing data.
  data_missing <- brew$data[[.col]]

  # get the name of the outcome column(s) for the brew
  outcome <- get_outcome(brew)$name

  # brew data will not have any missing values in outcome columns
  # so the outcome column needs to be taken out before scoring,
  # b/c it can't really be scored.
  data_missing[, outcome] <- NULL

  # If the complete data have the outcome column,
  # it needs to be removed as well.
  if(any(outcome %in% names(data_complete)))
    data_complete[, outcome] <- NULL

  # after dealing with outcomes, these datasets should have
  # the same names for each column.
  check_data_new_names(
    data_ref = brew$data[[.col]],
    data_new = data_complete
  )
  # data need to have the same types too
  check_data_new_types(
    data_ref = brew$data[[.col]],
    data_new = data_complete
  )

  new_col <- paste(.col, 'score', sep = '_')

  # compute a score for every training or testing set,
  # then attach that score to the corresponding row
  # of the wort
  brew$wort[[new_col]] <- brew$wort[[.col]] %>%
    purrr::map(vamp_vars,
      data_missing   = data_missing,
      data_complete  = data_complete,
      fun_ctns_error = fun_ctns_error,
      fun_intg_error = fun_intg_error,
      fun_bnry_error = fun_bnry_error,
      fun_catg_error = fun_catg_error
    )

  brew

}



