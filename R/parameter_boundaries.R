

get_par_bounds <- function(data, flavor){

  switch (flavor,
    'kneighbors' = get_par_bounds_knn(data),
    'softImpute' = get_par_bounds_sft(data)
  )

}

get_par_bounds_knn <- function(data){

  max_neighbors <- min(
    purrr::map_int(data, ~sum(stats::complete.cases(.x)))
  )

  list(neighbors = list(min=1, max=max_neighbors))

}

get_par_bounds_sft <- function(data){

  max_rank = min( dim(data) - 1 )

  list(
    rank_max_init = list(min = 2, max = max_rank),
    rank_max_ovrl = list(min = 2, max = max_rank),
    rank_stp_size = list(min = 1),
    lambda = list(min = 0)
  )

}
