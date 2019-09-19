

#' soft imputation.
#'
#' @description an adaptation of the soft impute algorithm for
#'   multiply imputed data.
#'
#' @param data data.frame or matrix
#'
#' @param n_impute An integer indicating the number of multiply imputed
#'   datasets to create.
#'
#' @param step_size An integer indicating the number by which to increase
#'   the maximum rank of the softImpute solution after each iteration.
#'
#' @param verbose `TRUE` or `FALSE`. if `TRUE`, output will be
#'   printed to the console indicating the ranks of solutions found
#'   for the softImpute fits.
#'
#' @inheritDotParams softImpute::softImpute type thresh maxit
#'   trace.it final.svd
#'
#' @note see [softimpute][softImpute::softImpute()] for a more
#'   descriptive summary of the `softImpute` algorithm.
#'
#' @return a `list` of soft fit objects
#'
#' @author Trevor Hastie, Rahul Mazumder
#'
#' @references Rahul Mazumder, Trevor Hastie and Rob Tibshirani (2010)
#'   Spectral Regularization Algorithms for Learning Large Incomplete
#'   Matrices, http://www.stanford.edu/~hastie/Papers/mazumder10a.pdf
#'   *Journal of Machine Learning Research* 11 (2010) 2287-2322
#'
#' @export
#'

# data = data$train %>% spread_cats()
# outcome = c("time","status")
# n_impute = 20
# step_size = 1
# scale_data = T
# scale_iter = 20
# .dots = list()
# scale_lambda = 0.95
# lambda_sequence = NULL
# verbose = TRUE

soft_fit <- function(
  data,
  outcome,
  n_impute = 10,
  step_size = 1,
  scale_data = TRUE,
  scale_iter = 20,
  scale_lambda = 0.95,
  lambda_sequence = NULL,
  verbose = TRUE,
  ...
){

  if(!all(outcome %in% names(data))){
    stop('outcome is not in the data')
  }

  new_data <- data

  new_data[, outcome] = NULL

  numeric_cols <- map_lgl(new_data, is.numeric)

  if(!all(numeric_cols)){

    stop(
      paste(
        "All columns should be numeric. Check the following:",
        list_things(names(numeric_cols)[!numeric_cols])
      )
    )

  }

  .dots <- list(...) %>%
    check_dots(
      valid_args = c(
        'type',
        'thresh',
        'maxit',
        'trace.it',
        'final.svd'
      )
    )

  lam0 <- lambda0(x = new_data) * scale_lambda
  lamseq <- lambda_sequence %||% seq(lam0, 1, length = n_impute)
  n_impute <- n_impute %||% length(lamseq)

  fits = as.list(lamseq)
  ranks = as.integer(lamseq)
  rank.max = rank_max = min(dim(new_data)) - 1
  warm = NULL

  .dots$x <- as.matrix(new_data)

  if(scale_data){

    if(verbose){
      message("Applying biScale() to data")
    }

    .dots$x %<>% biScale(maxit = scale_iter, trace = verbose)

  }

  for( i in seq_along(lamseq) ){

    .dots$lambda <- lamseq[i]
    .dots$rank.max = rank.max
    .dots$warm.start = warm

    fiti <- do.call(softImpute, args = .dots)

    attr(fiti, 'rank') <- ranks[i] <- sum(round(fiti$d, 4) > 0)
    rank.max = min(ranks[i] + step_size, rank_max)
    warm = fiti
    fits[[i]] = fiti

    if(verbose){

      message(
        glue(
          "fit {i} of {n_impute}: \\
          lambda = {format(round(lamseq[i], 3),nsmall=3)}, \\
          rank.max = {rank.max} \\
          rank.fit = {ranks[i]}"
        )
      )

    }

  }

  fits

}


#'  soft imputation.
#'
#' @description an adaptation of the soft impute algorithm for
#'   multiply imputed data.
#'
#' @param fits an object returned from [soft_fit()]
#'
#' @param new_data object to be imputed
#'
#' @note see [softimpute][softImpute::softImpute()] for a more
#'   descriptive summary of the `softImpute` algorithm.
#'
#' @return a [tibble][tibble::tibble-package] comprising the
#'   multiply imputed data, which are identified by the `.impute`
#'   column
#'
#' @author Trevor Hastie, Rahul Mazumder
#'
#' @references Rahul Mazumder, Trevor Hastie and Rob Tibshirani (2010)
#'   Spectral Regularization Algorithms for Learning Large Incomplete
#'   Matrices, http://www.stanford.edu/~hastie/Papers/mazumder10a.pdf
#'   *Journal of Machine Learning Research* 11 (2010) 2287-2322
#'
#' @export
#'

soft_fill <- function(
  fits,
  new_data
) {

  new_data %<>% as.matrix()

  output <- map(
    .x = fits,
    .f = ~ softImpute::complete(new_data, object = .x, unscale = TRUE)
  )

  output

}


#' mode estimation
#'
#' (copied from recipes)
#'
mode_est <- function (x)
{
  if (!is.character(x) & !is.factor(x))
    stop(
      "The data should be character or factor to compute the mode.",
      call. = FALSE
    )

  tab <- table(x)
  modes <- names(tab)[tab == max(tab)]
  sample(modes, size = 1)

}

nn_pred <- function(index, dat, k, random = FALSE) {

  dat <- dat[index[1:k], ]
  dat <- getElement(dat, names(dat))
  dat <- dat[!is.na(dat)]

  if(random){
    return(sample(dat, 1))
  }

  if (is.factor(dat) | is.character(dat)){
    return(mode_est(dat))
  }

  mean(dat)

}

#' k-nearest-neighbor (knn) imputation
#' @export

knn_fill <- function(
  data,
  new_data,
  outcome,
  n_impute = 10,
  step_size = 1,
  neighbor_sequence = NULL,
  verbose = TRUE,
  ...
) {

  .dots <- list(...) %>%
    check_dots(valid_args = c('eps', 'nthread'))

  if( !('eps' %in% names(.dots)) ){
    .dots$eps <- 1e-08
  }

  if( !('nthread' %in% names(.dots)) ){
    .dots$nthread <- getOption("gd_num_thread")
  }


  if(nrow(data) == 0 || ncol(data)==0){
    stop("data is empty", call.=FALSE)
  }

  if(nrow(new_data) == 0 || ncol(new_data)==0){
    stop("new_data is empty", call.=FALSE)
  }

  neighbor_sequence <- neighbor_sequence %||%
    seq(
      from = step_size,
      to = n_impute * step_size,
      by = step_size
    )

  output <- knn_work(
    ref_data = data,
    new_data = new_data,
    outcome = outcome,
    neighbor_sequence = neighbor_sequence,
    nthread = .dots$nthread,
    epsilon = .dots$eps,
    verbose = verbose
  )

}

#' k-nearest-neighbor (knn) imputation
#' @description  Adapted from Max Kuhn's knn imputation functions in recipes
#' @export

knn_fit <- function(
  data,
  outcome,
  n_impute = 10,
  step_size = 1,
  neighbor_sequence = NULL,
  verbose = TRUE,
  ...
) {

  .dots <- list(...) %>%
    check_dots(valid_args = c('eps', 'nthread'))

  if( !('eps' %in% names(.dots)) ){
    .dots$eps <- 1e-08
  }

  if( !('nthread' %in% names(.dots)) ){
    .dots$nthread <- getOption("gd_num_thread")
  }


  if(nrow(data) == 0 || ncol(data)==0){
    stop("data is empty", call.=FALSE)
  }

  neighbor_sequence <- neighbor_sequence %||%
    seq(
      from = step_size,
      to = n_impute,
      by = step_size
    )

  output <- knn_work(
    ref_data = data,
    outcome = outcome,
    neighbor_sequence = neighbor_sequence,
    nthread = .dots$nthread,
    epsilon = .dots$eps,
    verbose = verbose
  )

}


knn_work <- function(
  ref_data,
  new_data = NULL,
  outcome,
  neighbor_sequence,
  nthread = 1,
  epsilon = 1e-08,
  verbose = TRUE
) {

  new_data <- new_data %||% ref_data

  n_impute <- length(neighbor_sequence)

  missing_rows <- !complete.cases(new_data)
  if (!any(missing_rows))
    return(new_data)

  fits <- vector(mode = 'list', length = n_impute)

  for( i in seq(n_impute) ){
    fits[[i]] <- new_data
  }

  for ( i in seq(ncol(ref_data)) ) {

    imp_var <- names(new_data)[i]
    missing_rows <- !complete.cases(new_data[, imp_var])

    if ( any(missing_rows) ) {

      preds <- names(new_data)[-i]
      preds <- preds[-which(preds %in% outcome)]
      imp_data <- new_data[missing_rows, preds, drop = FALSE]

      ## do a better job of checking this:
      if (all(is.na(imp_data))) {

        warning(
          "All predictors are missing; cannot impute",
          call. = FALSE
        )

      } else {

        imp_var_complete <- !is.na(ref_data[[imp_var]])
        n_complete <- sum(imp_var_complete)

        n <- min(
          n_complete,
          max(neighbor_sequence)
        )

        if(verbose){

          nthings <- min(length(preds), 4)

          print(
            glue(
              "imputing {imp_var} (nobs = {n_complete}) using \\
              {glue_collapse(preds[1:nthings], sep = ', ')}, \\
              and others"
            )
          )

        }

        nn_index <- gower_topn(
          x = imp_data[, preds],
          y = ref_data[imp_var_complete, preds],
          n = n,
          nthread = nthread,
          eps = epsilon
        )$index

        pred_vals <- map(
          .x = neighbor_sequence,
          .f = ~ {

            if(verbose){
              print(glue("computing values using {.x} neighbors"))
            }

            apply(
              X = nn_index,
              MARGIN = 2,
              FUN = nn_pred,
              dat = ref_data[imp_var_complete, imp_var],
              k = .x
            )

          }
        )

        for(j in 1:n_impute){
          fits[[j]][missing_rows, imp_var] <- pred_vals[[j]]
        }

      }
    }
  }

  fits

}



# format_data_list(fits) %>%
#   mutate(neighbors = neighbor_sequence)
#
# format_data_list() %>%
#   mutate(
#     lambda = map_dbl(fits, ~attr(.x, 'lambda')),
#     rank = map_dbl(fits, ~attr(.x, 'rank'))
#   ) %>%
#   select(impute, lambda, rank, data)
