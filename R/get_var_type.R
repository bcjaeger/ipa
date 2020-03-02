

get_var_type <- function(x){

  if(is.double(x)){
    return('ctns')
  }

  if(is.integer(x)){
    return('intg')
  }

  if(is.factor(x)){

    n_lvls <- length(levels(x))

    if(n_lvls == 2){
      return('bnry')
    } else {
      return('catg')
    }

  }

  stop("incompatible variable type <", class(x)[1], "> in data")

}



