

fill_na <- function(data, vals){

  new_dt <- copy(data)

  for(col in names(vals)){

    na_indx <- which(is.na(new_dt[[col]]))

    if(length(na_indx) != length(vals[[col]])){
      stop(glue::glue(
        "data${col} has <{length(na_indx)}> missing values",
        " but vals${col} has <{length(vals[[col]])}> values"
      ), call. = FALSE)
    }

    if(!purrr::is_empty(na_indx)){
      set(new_dt, i = na_indx, j = col, value = vals[[col]])
    }

  }

  new_dt

}


