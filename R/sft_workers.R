
softImpute_work <- function(
  data,
  type,
  maxit,
  thresh,
  trace.it,
  rank_max,
  step_size,
  verbose_1,
  verbose_2,
  final.svd,
  scale_data,
  scale_iter,
  lambda_sequence
){

  if(!is.matrix(data)) data <- as.matrix(data)

  # Containers for results
  n_fits <- length(lambda_sequence)
  fit_args <- fits <- vector(mode = 'list', length = n_fits)
  max_ranks <- vector(mode = 'integer', length = n_fits)

  if(scale_data){

    if(verbose_1) message("Applying biScale() to data")

    data <- try(
      softImpute::biScale(
        x = data,
        maxit = scale_iter,
        trace = verbose_1
      ),
      silent = TRUE
    )

    if(class(data)[1] == 'try-error') stop(
      "unable to run biScale on brew data",
      call. = FALSE
    )

  }

  # Initial args
  args <- list(
    x = data,
    type = type,
    thresh = thresh,
    maxit = maxit,
    trace.it = trace.it,
    final.svd = final.svd
  )

  # These get updated in the for-loop below
  # They are left out of args for now on purpose
  # (makes it easier to add them in the for-loop)
  rank.max <- rank_max
  warm = NULL

  if(verbose_1) message("Fitting soft-impute models")

  for( i in seq(n_fits) ){

    # Update args
    args$lambda <- lambda_sequence[i]
    # rank.max and warm are updated at the end of the for-loop
    # then the values are carried over and plugged into args
    # at the beginning of the for-loop.
    args$rank.max <- rank.max
    args$warm.start <- warm

    fit <- do.call(softImpute::softImpute, args = args)

    # Determine the rank of the fit, which may or may not
    # be as high as the maximum rank.
    attr(fit, 'rank') <- sum(round(fit$d, 4) > 0)

    # update rank.max and warm
    rank.max <- min(attr(fit, 'rank') + step_size, rank_max)
    warm <- fit

    # Update containers
    max_ranks[i] = rank.max
    fits[[i]] = fit
    # args has two heavy objects we don't need
    keep_out <- which(names(args) %in% c('x','warm.start'))
    fit_args[[i]] <- args[-keep_out]

    if(verbose_1){

      print(
        glue::glue(
          "fit {i} of {n_fits}: \\
            lambda = {format(round(lambda_sequence[i], 3),nsmall=3)}, \\
            rank.max = {rank.max} \\
            rank.fit = {attr(fit, 'rank')}"
        )
      )

    }

  }

  tibble::tibble(impute = seq(n_fits), fit = fits, args = fit_args)

}
