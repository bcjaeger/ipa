

get_par_bounds <- function(data, flavor){

  switch (flavor,
    'kneighbors' = get_par_bounds_knn(data),
    'softImpute' = get_par_bounds_sft(data),
    'missRanger' = get_par_bounds_rgr(data)
  )

}

get_par_bounds_knn <- function(data){

  max_neighbors <- min(
    purrr::map_int(data, ~sum(stats::complete.cases(.x)))
  )

  list(
    neighbors = list(min=1, max=max_neighbors)
  )

}

get_par_bounds_sft <- function(data){

  max_rank = min( dim(data) - 1 )

  list(
    rank = list(min=1, max=max_rank)
  )

}

get_par_bounds_rgr <- function(data){

  max_node_size = floor(nrow(data) / 2)

  list(
    node_size = list(min=1, max=max_node_size)
  )

}
