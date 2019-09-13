

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

# data = trn
# n_impute = 20
# step_size = 1
# scale_data = FALSE
# .dots = list()

soft_fit <- function(
  data,
  outcome,
  n_impute = 10,
  step_size = 1,
  scale_data = TRUE,
  scale_lambda = 0.95,
  lambda_sequence = NULL,
  verbose = TRUE,
  ...
){

  if(!(outcome %in% names(data))){
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

  if(scale_data) .dots$x %<>% biScale()

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

      print(
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
      to = n_impute,
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
      preds <- preds[-which(preds==outcome)]
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


#' transfer factor levels
#'
#' @description take the factor levels in training data
#'   and copy them over to testing data. This is an important
#'   pre-processing step for data splits that may have
#'   different factor levels in training and testing sets.
#'
#' @param to the data that factor levels are transferred to
#' @param from the data that factor levels are transferred from
#'
#' @note `to` and `from` must have the same factor columns. For example,
#'   if `to` has a factor named `A` and `from` does not have a factor
#'   of the same name, the function will stop and tell you which
#'   factor variables are missing.
#'
#' @export
#'
transfer_factor_levels <- function(to, from){

  # check that the two frames have the same factor variables

  fctrs_to <- get_factors(to)
  fctrs_from <- get_factors(from)

  fctrs_only_in_to <- setdiff(fctrs_to, fctrs_from)
  fctrs_only_in_from <- setdiff(fctrs_from, fctrs_to)

  if(!is_empty(fctrs_only_in_to)){
    stop(
      paste(
        "to some factors that are not in from:",
        list_things(fctrs_only_in_to)
      )
    )
  }

  if(!is_empty(fctrs_only_in_from)){
    stop(
      paste(
        "from has some factors are not in to:",
        list_things(fctrs_only_in_from)
      )
    )
  }

  levels_from <- map(
    .x = set_names(fctrs_from, fctrs_from),
    .f = ~ levels(from[[.x]])
  )

  for(f in names(levels_from)){
    to[[f]] %<>% factor(levels = levels_from[[f]])
  }

  return(to)

}


#' Prepare data stacks/lists
#'
#' @description stacked dataframes comprising multiply imputed
#'   dataframes stacked on top of one another. Imputed values in
#'   these objects are usually a model prediction + random noise.
#'   The additional noise creates diversity among the multiple
#'   dataframes. Data lists are similar, but data frames are kept
#'   separate and one model is fit for each data set.
#'
#'   These functions create ID variables and attach classes and attributes
#'   that are used to direct downstream analyses.
#'
#' @param data This argument should be a list a list of
#'   imputed dataframes, each having the same number of rows and columns.
#'
#' @export
#'
#' @examples
#' data_list <- list(
#'   df1 = data.frame(a = 1:3, b = letters[1:3]),
#'   df2 = data.frame(a = 2:4, b = letters[2:4])
#' )
#'
#' as_data_stack(data_list)
#'
#' as_mi(data_list)
#'
#' as_si(data_list[[1]])

as_data_stack <- function(data){

    nimpute <- length(data)

    data %<>% map(
      .f = ~ .x %>%
        ungroup() %>%
        mutate(._ID_. = 1:n())
    ) %>%
      bind_rows() %>%
      as_tibble() %>%
      arrange(._ID_.) %>%
      select(._ID_., everything()) %>%
      set_miss_strat(miss_strat = 'midy')

    attr(data, 'nimpute') <- nimpute

    data

}

#' @rdname as_data_stack
#' @export
as_data_list <- function(data){

  nimpute <- length(data)

  data %<>%
    bind_rows(.id = '._ID_.') %>%
    as_tibble() %>%
    select(._ID_., everything()) %>%
    set_miss_strat(miss_strat = 'mi')

  attr(data, 'nimpute') <- nimpute

  data

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
