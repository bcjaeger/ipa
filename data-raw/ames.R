

## code to prepare `amesHousing` dataset goes here

set.seed(329)

# library(tidyverse)
# library(AmesHousing)

ames_complete <- ames_missing <- drop_na(make_ames())
rows <- sample(nrow(ames_complete), 1200)

for(i in rows){

  cols <- sample(ncol(ames_complete)-1, size = ncol(ames_complete) - 5)
  ames_missing[i, cols] <- NA

}



ames <- list(
  complete = ames_complete,
  missing = ames_missing
) %>%
  map(mutate, Sale_Price = log(Sale_Price)) %>%
  map(mutate, Kitchen_AbvGr = factor(Kitchen_AbvGr)) %>%
  map(select, Sale_Price, everything(), -Utilities)

ames$missing$Sale_Price <- ames$complete$Sale_Price

usethis::use_data(ames, overwrite = TRUE)
