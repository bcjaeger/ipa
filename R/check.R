
#' check positive integer input
check_pos_int <- function(x, label){

  if(x <= 0)
    stop(glue("{label} should be > 0"), call. = FALSE)

  .int <- as.integer(x)

  if(x != .int)
    stop(glue('{label} should be an integer'), call. = FALSE)

}

#' check step size for soft imputes
check_step_size <- function(step_size, n_impute, max_rank){

  expected_max_rank <- step_size * n_impute

  observed_max_rank <- as.integer(floor(max_rank / n_impute))

  if(expected_max_rank > max_rank) {
    out_msg <- glue(
      "step_size is too big. to create {n_impute} imputations \\
      of the given data, maximum step size is {observed_max_rank}"
    )
    stop(out_msg, call. = FALSE)
  }

}


#' check inputs
check_dots <- function(.dots, valid_args){

  bad_args <- setdiff(names(.dots), valid_args)

  if(!is_empty(bad_args)){
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

#' check missing strategy input
check_miss_strat <- function(miss_strat){

  valid_input <- miss_strat %in% c('si','mi','stacked')

  if(!valid_input){
    stop("miss_strat should be one of 'si', 'mi', or 'stacked'")
  }

}

# check xgb inputs for stacked mi
check_xgb_stack_args <- function(args, n_impute){

  if( is.null(n_impute) ){
    stop("n_impute needs to be specified", call. = FALSE)
  }

  if(!inherits(args$params, 'stacked_params')){
    args$params %<>% stack_params(n_impute = n_impute)
  }

  if(!inherits(args$label, 'stacked_label')){
    args$label %<>% stack_label(n_impute = n_impute)
  }

  if('folds' %in% names(args)){
    if(!inherits(args$folds, 'stacked_folds')){
      args$folds %<>% stack_folds(n_impute = n_impute)
    }
  }

  args

}


check_ferment_data <- function(brew, new_data){

  new_names <- names(new_data)
  old_names <- names(brew$data)

  same_size <- length(new_names) == length(old_names)

  if(!same_size){

    too_many_new <- length(new_names) > length(old_names)

    if(too_many_new){

      out_msg <- glue(
        "new data have columns not contained in brew data: \\
        {list_things(setdiff(new_names, old_names))}"
      )

    } else {

      out_msg <- glue(
        "brew data have columns not contained in new data: \\
        {list_things(setdiff(old_names, new_names))}"
      )

    }

    stop(out_msg, call. = FALSE)

  }


}

check_malt <- function(malt){
  if(is.null(attr(malt, 'malted'))) stop(
    "the brew has not been mashed! Try using soft_malt() before boiling",
    call. = FALSE
  )
}


check_mash <- function(mash){
  is_not_mashed <- !attr(mash, 'mashed')

  if(is_not_mashed) stop(
    "the brew has not been mashed! Try using soft_mash() before boiling",
    call. = FALSE
  )
}

check_brew <- function(brew, col_name){

  # check to make sure the object has been
  # passed through the right functions before
  # it got passed here

  is_not_boiled <- !attr(brew, 'boiled')
  if(is_not_boiled)
    stop(
      "the brew has not been boiled! Try using boil() before fermenting",
      call. = FALSE
    )

  if(tolower(col_name) %in% c('train', 'training')) stop(
    "training data is automatically imputed. ",
    "col_name should specify label for testing data",
    call. = FALSE
  )

}

