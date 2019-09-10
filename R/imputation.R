

#' soft imputation.
#'
#' @description an adaptation of the soft impute algorithm for
#'   multiply imputed data.
#'
#' @param data data.frame or matrix
#'
#' @param n_lambda An integer indicating the number of multiply imputed
#'   datasets to create.
#'
#' @param rank_step An integer indicating the number by which to increase
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
# n_lambda = 20
# rank_step = 1
# scale_data = FALSE
# .dots = list()

soft_fit <- function(
  data,
  n_lambda = 10,
  scale_lambda = 0.95,
  lambda_sequence = NULL,
  rank_step = 1,
  scale_data = TRUE,
  verbose = TRUE,
  ...
){

  numeric_cols <- map_lgl(data, is.numeric)

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

  lam0 <- lambda0(x = data) * scale_lambda
  lamseq <- lambda_sequence %||% seq(lam0, 1, length = n_lambda)
  n_lambda <- n_lambda %||% length(lamseq)

  fits = as.list(lamseq)
  ranks = as.integer(lamseq)
  rank.max = rank_max = min(dim(data)) - 1
  warm = NULL

  .dots$x <- as.matrix(data)

  if(scale_data) .dots$x %<>% biScale()

  for( i in seq_along(lamseq) ){

    .dots$lambda <- lamseq[i]
    .dots$rank.max = rank.max
    .dots$warm.start = warm

    fiti <- do.call(softImpute, args = .dots)

    attr(fiti, 'rank') <- ranks[i] <- sum(round(fiti$d, 4) > 0)
    rank.max = min(ranks[i] + rank_step, rank_max)
    warm = fiti
    fits[[i]] = fiti

    if(verbose){

      print(
        glue(
          "fit {i} of {n_lambda}: \\
          lambda = {format(round(lamseq[i], 3),nsmall=3)}, \\
          rank.max = {rank.max} \\
          rank.fit = {ranks[i]}"
        )
      )

    }

  }

  fits

}

#' k-nearest-neighbor (knn) imputation
#' @export

knn_fit <- function(
  data,
  vars = NULL,
  k_neighbors = 12,
  k_step = 2,
  ...
){

  if(k_neighbors %% k_step != 0){
    stop("k_step should evenly divide k_neighbors", call. = FALSE)
  }

  n_step <- k_neighbors / k_step

  .vars <- vars %||% colnames(data)

  .dots <- list(...) %>%
    check_dots(
      valid_args = c("eps", "weights", "ignore_case", "nthread")
    )

  .dots$x <- .dots$y <- data[, .vars]
  .dots$n <- nrow(data)

  gower <- do.call(gower_topn, args = .dots)

  obs_index <- map(data, ~which(!is.na(.x)))

  imputes <- tibble(
    row_index = which(!complete.cases(data)),
    col_index = map(row_index, ~which(is.na(data[.x, ])))
  ) %>%
    unnest(col = col_index) %>%
    mutate(
      vals = map2(
        .x = row_index,
        .y = col_index,
        .f = ~ {
          .rows <- intersect(gower$index[,.x], obs_index[[.y]])
          .stop <- min(k_neighbors, length(.rows))
          .vals <- data[.rows[1:.stop], .y, drop=TRUE]
          .gwrd <- gower$distance[obs_index[[.y]], .x][1:.stop]
          list(values = .vals, gower_d = .gwrd)
        }
      )
    ) %>%
    unnest_wider(col = vals) %>%
    add_class("fit_knn")

  output <- vector(mode='list', length = n_step)

  for( i in seq_along(output) ){

    output[[i]] <- data
    n_neighbors <- i * k_step

    for(j in 1:nrow(imputes)){

      output[[i]][imputes$row_index[j], imputes$col_index[j]] <-
        if(is.numeric(imputes$values[[j]])){

          mean(imputes$values[[j]][1:n_neighbors])

        } else {

          names(which.max(table(imputes$values[[j]][1:n_neighbors])))

        }

    }

  }

  output %>%
    format_data_list() %>%
    mutate(k_neighbors = seq(k_step, k_neighbors, by = k_step)) %>%
    select(impute, k_neighbors, data)

}


#'  soft imputation.
#'
#' @description an adaptation of the soft impute algorithm for
#'   multiply imputed data.
#'
#' @param data object to be imputed
#'
#' @param fits an object returned from [soft_fit()]
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
  data,
  fits
) {

  data %<>% as.matrix()

  output <- map(
    .x = fits,
    .f = ~ softImpute::complete(data, object = .x, unscale = TRUE)
  ) %>%
    format_data_list() %>%
    mutate(
      lambda = map_dbl(fits, ~attr(.x, 'lambda')),
      rank = map_dbl(fits, ~attr(.x, 'rank'))
    ) %>%
    select(impute, lambda, rank, data)

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


#' Prepare `si`/`mi`/`midy` data
#'
#' @description `si` objects are singly imputed dataframes where
#'   missing values are filled in with a model's predicted value.
#'   `mi` and `midy` objects are dataframes comprising multiply imputed
#'   dataframes stacked on top of one another. Imputed values in
#'   `mi` and `midy` objects are a model prediction + random noise.
#'   The additional noise creates diversity among the multiple
#'   dataframes. Data in `mi` objects are sorted by `._mi.ID_.`,
#'   which identifies dataframes. Data in `midy` objects are sorted
#'   by `._midy.ID_.`, which identifies rows in the original (unimputed)
#'   data.
#'
#'   These functions create ID variables and attach classes and attributes
#'   that are used to direct downstream analyses.
#'
#' @param data This argument should be a list a list of
#'   imputed dataframes, each having the same number of rows and columns,
#'   for `as_midy` and `as_mi`. For `as_si`, this argument should be
#'   a data frame with imputed values.
#'
#' @note Imputed dataframes are copies of an original dataframe,
#'   with cells that originally had missing values filled in
#'   with a plausible estimate.
#'
#' @export
#'
#' @examples
#' data_list <- list(
#'   df1 = data.frame(a = 1:3, b = letters[1:3]),
#'   df2 = data.frame(a = 2:4, b = letters[2:4])
#' )
#'
#' as_midy(data_list)
#'
#' as_mi(data_list)
#'
#' as_si(data_list[[1]])

as_midy <- function(data){

    nimpute <- length(data)

    data %<>% map(
      .f = ~ .x %>%
        ungroup() %>%
        mutate(._midy.ID_. = 1:n())
    ) %>%
      bind_rows() %>%
      as_tibble() %>%
      arrange(._midy.ID_.) %>%
      select(._midy.ID_., everything()) %>%
      set_miss_strat(miss_strat = 'midy')

    attr(data, 'nimpute') <- nimpute

    data

}

#' @rdname as_midy
#' @export
as_mi <- function(data){

  nimpute <- length(data)

  data %<>%
    bind_rows(.id = '._mi.ID_.') %>%
    as_tibble() %>%
    select(._mi.ID_., everything()) %>%
    set_miss_strat(miss_strat = 'mi')

  attr(data, 'nimpute') <- nimpute

  data

}




