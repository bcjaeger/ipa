


#' Sip a brew
#'
#' @inheritParams scrimp_vars
#'
#' @param brew an `ipa_brew` object.
#'
#' @param from column name for sipping. Valid options are
#'   training and testing. Inputs can be quoted (e.g. 'training')
#'   or unquoted (e.g. training).
#'
#' @return an `ipa_brew` object with a new column added to the `wort`.
#'   The new column contains a list of [tibble::tibble()]s with columns
#'   `variable`, `type`, and `score`. The `score` column comprises
#'   output from the `error` functions.
#'
#' @export
#'
#' @examples
#'
#' data("diabetes")
#'
#' df_miss <- diabetes$missing
#' df_cplt <- diabetes$complete
#'
#' sft_brew <- brew_soft(df_miss, outcome = diabetes) %>%
#'   mash() %>%
#'   stir() %>%
#'   ferment() %>%
#'   bottle(type = 'tibble')
#'
#'
#' sip(sft_brew, from = training, data_complete = df_cplt)
#'


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
    purrr::set_names(NULL)

  check_l1_stop(.col, label = 'column to sip from')

  # if we are scoring training data, then we need to look at
  # the brew's training data. Same thing goes for scoring testing data.
  data_missing <- cbind(
    attr(brew, 'outcome')[[.col]],
    brew$data[[.col]]
  )

  check_data_new_names(
    data_ref = data_missing, label_ref = 'brew data',
    data_new = data_complete, label_new = 'complete data'
  )

  # data need to have the same types too
  check_data_new_types(
    data_ref = data_missing, label_ref = 'brew data',
    data_new = data_complete, label_new = 'complete data'
  )

  new_col <- paste(.col, 'score', sep = '_')

  # compute a score for every training or testing set,
  # then attach that score to the corresponding row
  # of the wort

  set(brew$wort, j = new_col,
    value = lapply(
      X = brew$wort[[.col]],
      FUN = scrimp_vars,
      data_missing   = data_missing,
      data_complete  = data_complete,
      fun_ctns_error = fun_ctns_error,
      fun_intg_error = fun_intg_error,
      fun_bnry_error = fun_bnry_error,
      fun_catg_error = fun_catg_error
    )
  )

  brew

}



