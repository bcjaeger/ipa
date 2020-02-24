
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

# Check the flavor

check_flavor <- function(flavor){

  good_flavors <- c('kneighbors','softImpute','missRanger')
  glue_flavors <- glue::glue_collapse(good_flavors, sep = ', ', last = ', or ')

  if( !(flavor %in% good_flavors) ){
    stop('flavor should be one of ', glue_flavors, call. = FALSE)
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
check_data_new_names <- function(data_ref, data_new){

  new_names <- names(data_new)
  ref_names <- names(data_ref)

  list_new <- !(new_names %in% ref_names)
  list_ref <- !(ref_names %in% new_names)

  error_new <- any(list_new)
  error_ref <- any(list_ref)

  if(error_new){
    out_msg_new <- paste(
      "new data have columns not contained in reference data:",
        list_things(new_names[list_new])
    )
  }

  if(error_ref){
    out_msg_ref <- paste(
      "reference data have columns not contained in new data:",
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

check_data_new_types <- function(data_ref, data_new){

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
        "{types$name[.x]} has type <{types$ref[.x]}> in reference data \\
        and type <{types$new[.x]}> in new data."
      )
    ) %>%
      glue::glue_collapse(sep = '\nAlso, ')

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

# make sure data ref can be imputed
check_data_ref <- function(data, cols){

  # Check for empty rows/cols
  all_rows_na <- apply(
    X = data[, cols, drop = FALSE],
    MARGIN = 1L,
    FUN = function(x) all(is.na(x))
  )

  all_cols_na <- apply(
    X = data[, cols, drop = FALSE],
    MARGIN = 2L,
    FUN = function(x) all(is.na(x))
  )

  if (any(all_rows_na)) {
    stop("some rows are missing data for all predictors: ",
      glue::glue_collapse(which(all_rows_na), sep = ', ', last = ' and '),
      call. = FALSE)
  }

  if (any(all_cols_na)) {
    stop("some columns are missing data for all values: ",
      glue::glue_collapse(names(data)[all_cols_na], sep=', ', last=' and '),
      call. = FALSE)
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


check_var_types <- function(data, valid_types){

  var_types <- purrr::map_chr(data, ~ class(.x)[1])

  good_vars <- var_types %in% valid_types

  if(!all(good_vars)){

    bad_vars <- which(!good_vars)

    msg <- paste("some variables have unsupported type: ",
      list_things(names(var_types)[bad_vars]),
      '\n supported types are',
      list_things(valid_types)
    )

    stop(msg, call. = FALSE)

  }

}

