


#' Stir your brew
#'
#' After setting your primary and secondary imputation parameters
#'   (i.e., running [spice] and [mash]), you're ready to fit the
#'   imputation model(s) that will impute missing values in your
#'   training data. This is what the `stir` function does.
#'
#' @param brew an `ipa_brew` object.
#'
#' @param timer a logical value. If `TRUE`, then the amount of time it takes
#'   to fit the imputation models will be tracked and saved as an attribute
#'   of the resulting `ipa_brew` object.
#'
#' @return an `ipa_brew` object with imputed values added to the `wort`
#'
#' @export
#'
#' @examples
#'
#' x1 = rnorm(100)
#' x2 = rnorm(100) + x1
#' x3 = rnorm(100) + x1 + x2
#'
#' outcome = 0.5 * (x1 - x2 + x3)
#'
#' n_miss = 10
#' x1[1:n_miss] <- NA
#'
#' data <- data.frame(x1=x1, x2=x2, x3=x3, outcome=outcome)
#'
#' sft_brew <- brew_soft(data, outcome=outcome, bind_miss = FALSE)
#' sft_brew <- mash(sft_brew, with = masher_soft(bs = TRUE))
#' sft_brew <- stir(sft_brew, timer = TRUE)
#' sft_brew
#'
stir <- function(brew, timer = FALSE){

  impute_fun <- switch(get_flavor(brew),
    'kneighbors' = impute_nbrs,
    'softImpute' = impute_soft)

  if(timer){

    start <- Sys.time()

    if(get_verbosity(brew) > 0){

      .flavor <- switch(get_flavor(brew),
        'kneighbors' = 'k-nearest-neighbors',
        'softImpute' = 'soft imputation')

      message("Fitting ", .flavor, " models to training data...")

    }

  }

  brew$wort <- do.call(
    impute_fun,
    args = c(list(data_ref = brew$data$training), brew$pars)
  )

  setnames(brew$wort, old = 'imputed_values', new = 'iv_training')

  if(timer){

    stop <- Sys.time()
    dt_val <- as.difftime(stop-start)
    attr(brew, 'stir_time') <- dt_val

    if(get_verbosity(brew) > 0){
      dt_msg <- paste(round(dt_val, 2), attr(dt_val, 'units'))
      message("Finished after ", dt_msg)
    }

  }

  attr(brew, 'stirred') <- TRUE

  brew

}
