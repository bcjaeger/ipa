## code to prepare `diabetes` dataset goes here

set.seed(329)

data("Diabetes", package = 'Publish')

diab_complete <- diab_missing <- as_tibble(Diabetes) %>%
  select(-id, -bp.2s, -bp.2d, -AgeGroups, -BMI) %>%
  rename(
    sbp = bp.1s,
    dbp = bp.1d,
    height_cm = height.europe,
    weight_kg = weight.europe,
    time_ppn = time.ppn,
    stable_glucose = stab.glu
  ) %>%
  mutate(diabetes = as.numeric(glyhb > 7)) %>%
  select(-glyhb, -location) %>%
  drop_na()


rows <- sample(nrow(diab_complete), 200)

for(i in rows){

  cols <- sample(ncol(diab_complete)-1, size = ncol(diab_complete) - 5)
  diab_missing[i, cols] <- NA

}

diab_complete %<>%
  data.table::as.data.table() %>%
  mltools::one_hot() %>%
  as_tibble() %>%
  select(-gender_female, -frame_large, -height, -weight) %>%
  mutate(diabetes = factor(diabetes, labels = c("No", "Yes"))) %>%
  select(diabetes, everything())

diab_missing %<>%
  data.table::as.data.table() %>%
  mltools::one_hot() %>%
  as_tibble() %>%
  select(-gender_female, -frame_large, -height, -weight) %>%
  mutate(diabetes = factor(diabetes, labels = c("No", "Yes"))) %>%
  select(diabetes, everything())

diabetes <- list(
  complete = diab_complete,
  missing = diab_missing
)

usethis::use_data(diabetes, overwrite = TRUE)

