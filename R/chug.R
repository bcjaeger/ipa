

#' Chug a brew
#'
#'
#' @param brew an `ipa_brew` object.
#' @inheritParams scrimp_mdl
#'
#' @return an `ipa_brew` object with a column added to the `wort`
#'   containing the output from [scrimp_mdl].
#' @export
#'
chug <- function(brew, .fun = NULL, .fun_args = NULL){

  check_brew(brew, expected_stage = 'chug')

  brew$wort$model_score <- purrr::map2(
    .x = brew$wort$training,
    .y = brew$wort$testing,
    .f = scrimp_mdl,
    outcome = get_outcome_name(brew),
    .fun_args = .fun_args,
    .fun = .fun
  )

  attr(brew, 'chugged') <- TRUE

  brew

}
