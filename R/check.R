
check_type <- function(x, label, type){

  .fun <- switch (type,
    'logical' = is.logical,
    'double' = is.double,
    'integer' = is.integer,
    'factor' = is.factor,
    stop('type is unrecognized', call. = FALSE)
  )

  .type <- class(x)[1]

  if(!.fun(x)) stop(glue::glue(
    "{label} must have type <{type}>, but it has type <{.type}>"),
    call. = FALSE)

}

# check integer input
check_int <- function(x, label){

  .int <- as.integer(x)

  if(any(x != .int))
    stop(glue::glue('{label} should be an integer'), call. = FALSE)

}

check_chr <- function(x, label, options){

  opts <- glue::glue_collapse(options, sep = ', ', last = ' or ')

  if( !(x %in% options) ){
    stop(glue::glue('{label} should be one of {opts}'), call. = FALSE)
  }

}

# check minimums input
check_min_strict <- function(x, label, value){
  if(any(x <= value)) stop(glue::glue("{label} should be > {value}"),
      call. = FALSE)
}
check_min_lax <- function(x, label, value){
  if(any(x < value)) stop(glue::glue("{label} should be >= {value}"),
      call. = FALSE)
}

# check maximum input
check_max_strict <- function(x, label, value){
  if(any(x >= value)) stop(glue::glue("{label} should be < {value}"),
    call. = FALSE)
}
check_max_lax <- function(x, label, value){
  if(any(x > value)) stop(glue::glue("{label} should be <= {value}"),
    call. = FALSE)
}

check_bool <- function(x, label){

  if(!is.logical(x)) stop(glue::glue("{label} should have type <logical>.",
    "\nInstead, it has type {class(x)[1]}"), call. = FALSE)

}

# check fraction input
check_fraction <- function(x, label){

  if(any(x > 1 || x < 0))
    stop('{label} should be > 0 and < 1', call.=FALSE)

}

# check step size for soft imputes
# TODO: remove this from softimpute brew
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

# check data passed to ferment
check_data_new_names <- function(data_ref, data_new,
  label_ref = 'reference data', label_new = 'new data'){

  new_names <- names(data_new)
  ref_names <- names(data_ref)

  list_new <- !(new_names %in% ref_names)
  list_ref <- !(ref_names %in% new_names)

  error_new <- any(list_new)
  error_ref <- any(list_ref)

  if(error_new){
    out_msg_new <- glue::glue(
      "{label_new} have columns not contained in {label_ref}: ",
        list_things(new_names[list_new])
    )
  }

  if(error_ref){
    out_msg_ref <- glue::glue(
      "{label_ref} have columns not contained in {label_new}: ",
      list_things(ref_names[list_ref])
    )
  }

  if(error_new && error_ref){
    out_msg <- c(out_msg_new, '\n Also, ', out_msg_ref)
  }

  if (error_new && !error_ref) {
    out_msg <- c(out_msg_new)
  }

  if (!error_new && error_ref){
    out_msg <- c(out_msg_ref)
  }

  any_error <- error_new | error_ref

  if(any_error){
    stop(out_msg, call. = FALSE)
  }

}

check_data_new_types <- function(data_ref, data_new,
  label_ref = 'reference data', label_new = 'new data'){

  # this assumes you have already run check_data_new_names

  new_types <- purrr::map_chr(data_new, ~class(.x)[1]) %>%
    tibble::enframe() %>%
    dplyr::mutate(type = 'new')

  ref_types <- purrr::map_chr(data_ref, ~class(.x)[1]) %>%
    tibble::enframe() %>%
    dplyr::mutate(type = 'ref')

  types <- dplyr::bind_rows(ref_types, new_types) %>%
    tidyr::pivot_wider(names_from = type, values_from = value)

  if(any(types$ref != types$new)){

    list_rows <- which(types$ref != types$new)

    out_msg <- purrr::map(
      .x = list_rows,
      .f = ~ glue::glue(
        "{types$name[.x]} has type <{types$ref[.x]}> in {label_ref} ",
        "and type <{types$new[.x]}> in {label_new}."
      )
    ) %>%
      glue::glue_collapse(sep = '\nAlso, ')

    stop(out_msg, call. = FALSE)

  }

}



check_l1_stop <- function(x, label){

  if(length(x) > 1) stop(glue::glue("{label} should have",
  " length of 1 but has length of {length(x)}."), call. = FALSE)

}

check_l1_warn <- function(x, label){

  if(length(x) > 1){

    warning(glue::glue("{label} should have",
      " length of 1 but has length of {length(x)}.",
      "\nOnly the first value will be used."), call. = FALSE)

    return(TRUE)

  } else {

    return(FALSE)

  }

}




# check that a brew is in the right stage
check_brew <- function(brew, expected_stage){

  if(!is_brew(brew)){
    stop("brew should be an ipa_brew object - see brew() function")
  }

  # don't need to include mash need b/c spicing is automated

  previous_stage <- switch(
    expected_stage,
    'spice' = 'initiated',
    'ferment' = 'stirred',
    'bottle' = 'fermented',
    'sip' = 'bottled',
    'chug' = 'bottled'
  )

  recommended_function <- switch(
    expected_stage,
    'spice' = 'brew',
    'ferment' = 'stir',
    'bottle' = 'ferment',
    'sip' = 'bottle',
    'chug' = 'bottle'
  )

  brew_checker <- switch(
    expected_stage,
    'spice' = is_brew,
    'ferment' = is_stirred,
    'bottle' = is_fermented,
    'sip' = is_bottled,
    'chug' = is_bottled
  )

  if(!brew_checker(brew))
    stop(glue::glue("the brew has not been {previous_stage}!\n",
      "Try using the {recommended_function}() function before ",
      "using the {expected_stage}() function"), call. = FALSE)


}

# make sure data ref can be imputed
check_missingness <- function(miss_indx, N, P, label){

  if(is_empty(miss_indx)) return(NULL)

  # No. of missing observations for each variable with >1 missings
  miss_nobs <- lapply(miss_indx, length)
  # the columns that contain only missing values
  miss_cols <- names( which( sapply(miss_nobs, function(x) x==N) ) )

  if(!is_empty(miss_cols)){
    stop("columns in ", label, " are missing data for all values: ",
      list_things(miss_cols), call. = FALSE)
  }

  # the rows that contain only missing values
  # only need to worry about these if
  if(length(miss_indx) == P){

    miss_rows <- Reduce(x = miss_indx, f = intersect)

    if(!is_empty(miss_rows)){
      stop("rows in ", label, " are missing data for all values: ",
        list_things(miss_rows), call. = FALSE)
    }

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


check_var_types <- function(data, valid_types){

  var_types <- purrr::map_chr(data, ~ class(.x)[1])

  good_vars <- var_types %in% valid_types

  if(!all(good_vars)){

    bad_vars <- which(!good_vars)

    vars_to_list <- names(var_types)[bad_vars]
    types_to_list <- var_types[vars_to_list]

    meat <- paste0('<', vars_to_list, '> has type <',
      types_to_list, '>', collapse = '\n')

    msg <- paste("some variables have unsupported type:\n",
      meat, '\n supported types are', list_things(valid_types)
    )

    stop(msg, call. = FALSE)

  }

}

