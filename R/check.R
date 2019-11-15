
check_data_list <- function(data_list){

  if(!is.list(data_list)){
    stop("data_list should be a list")
  }

  ncols <- purrr::map_int(data_list, ncol)
  nrows <- purrr::map_int(data_list, nrow)

  if(!all(ncols[-1] == ncols[1])){
    stop(
      "All data frames in `data_list` should have same number of columns"
    )
  }

  if(!all(nrows[-1] == nrows[1])){
    stop(
      "All data frames in `data_list` should have same number of rows"
    )
  }


}

# check positive integer input
check_pos_int <- function(x, label){

  if(x <= 0)
    stop(glue::glue("{label} should be > 0"), call. = FALSE)

  .int <- as.integer(x)

  if(x != .int)
    stop(glue::glue('{label} should be an integer'), call. = FALSE)

}

# check fraction input
check_fraction <- function(x, label){

  if(x > 1 || x < 0)
    stop('{label} should be > 0 and < 1', call.=FALSE)

}


check_step_size <- function(step_size, n_impute,  max_rank){

  expected_max_rank <- step_size * n_impute

  observed_max_rank <- as.integer(floor(max_rank / n_impute))

  if(expected_max_rank > max_rank) {
    out_msg <- glue::glue(
      "step_size or n_impute is too big. Try reducing n_impute or step_size"
    )
    stop(out_msg, call. = FALSE)
  }

}

# check inputs
check_dots <- function(.dots, valid_args){

  bad_args <- setdiff(names(.dots), valid_args)

  if(!purrr::is_empty(bad_args)){
    stop(
      paste(
        "The following arguments are unrecognized:",
        list_things(bad_args)
      ),
      call. = FALSE
    )
  }

  .dots

}

check_ferment_data <- function(brew, new_data){

  new_names <- names(new_data)
  old_names <- names(brew$data)

  same_size <- length(new_names) == length(old_names)

  if(!same_size){

    too_many_new <- length(new_names) > length(old_names)

    if(too_many_new){

      out_msg <- glue::glue(
        "new data have columns not contained in brew data: \\
        {list_things(setdiff(new_names, old_names))}"
      )

    } else {

      out_msg <- glue::glue(
        "brew data have columns not contained in new data: \\
        {list_things(setdiff(old_names, new_names))}"
      )

    }

    stop(out_msg, call. = FALSE)

  }


}


check_brew <- function(brew, expected_stage){

  if(!is_brew(brew)){
    stop("brew should be an ipa_brew object - see brew() function")
  }

  #don't to include mash need b/c spicing is automated

  previous_stage <- switch(
    expected_stage,
    'spice' = 'initiated',
    'ferment' = 'mashed',
    'bottle' = 'fermented'
  )

  recommended_function <- switch(
    expected_stage,
    'spice' = 'brew',
    'ferment' = 'mash',
    'bottle' = 'ferment'
  )

  brew_checker <- switch(
    expected_stage,
    'spice' = is_brew,
    'ferment' = is_mashed,
    'bottle' = is_fermented
  )

  if(!brew_checker(brew)){
    stop(
      glue::glue("the brew has not been {previous_stage}!\n",
        "Try using the {recommended_function}() function before ",
        "using the {expected_stage}() function")
    )
  }

}

check_spicer <- function(spicer, expected){

  if(is.null(spicer)){
    return(NULL)
  }

  if(!inherits(spicer, expected)) stop(
    glue::glue("The {class(spicer)[1]} spicer you have used is not ",
      "compatible with the {expected} brew you are making!",
    call. = FALSE)
  )

}
