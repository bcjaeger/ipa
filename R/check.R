
# check to make sure data are uniform before stacking
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

  if(any(x <= 0))
    stop(glue::glue("{label} should be > 0"), call. = FALSE)

  .int <- as.integer(x)

  if(any(x != .int))
    stop(glue::glue('{label} should be an integer'), call. = FALSE)

}

# check fraction input
check_fraction <- function(x, label){

  if(any(x > 1 || x < 0))
    stop('{label} should be > 0 and < 1', call.=FALSE)

}

# check step size for soft imputes
check_step_size <- function(step_size, n_impute,  max_rank){

  if(step_size * n_impute > max_rank) {
    stop("step_size or n_impute is too big. ",
      "Try reducing n_impute or step_size",
      call. = FALSE)
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

# check data passted to ferment
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

# check that a brew is in the right stage
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

# check spicer input
check_spicer <- function(spicer, expected){

  if(is.null(spicer)){
    return(NULL)
  }

  if(is_masher(spicer)) stop(
    "looks like you used a masher when you meant to use a spicer!",
    call. = FALSE
  )

  if(!inherits(spicer, expected)) stop(
    glue::glue("The {class(spicer)[1]} spicer you have used is not ",
      "compatible with the {expected} brew you are making!",
    call. = FALSE)
  )

}

# check mash - spice if needed
check_mash <- function(brew, verbose){

  if(!is_spiced(brew)){
    brew <- simple_spice(brew)
    if(verbose > 0) message(
      "Looks like this brew hasn't been spiced yet.\n",
      "I will spice it for you using default values.\n",
      "Take a look at <your brew>$pars to see these values.\n"
    )
  }

}

# check masher input
check_masher <- function(masher, expected){

  if(is.null(masher)){
    return(NULL)
  }

  if(is_spicer(masher)) stop(
    "looks like you used a spicer when you meant to use a masher!",
    call. = FALSE
  )

  if(!inherits(masher, expected)) stop(
    glue::glue("The {class(masher)[1]} masher you have used is not ",
      "compatible with the {expected} brew you are making!",
      call. = FALSE)
  )

}
