
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/bcjaeger/ipa.svg?branch=master)](https://travis-ci.org/bcjaeger/ipa)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

# Imputation for predictive analytics (`ipa`)

The goal of `ipa` is to make imputation in predictive modeling workflows
more straightforward and efficient. The main functions in `ipa` are

1.  `brew` Create a container to hold your imputations

2.  `spice` (optional) set parameters that govern the number of
    imputations for the given brew

3.  `mash` fit models that will provide imputations

4.  `ferment` impute missing values in training and (optionally) testing
    data

5.  `bottle` output the imputed datasets in a tibble.

## Installation

<!-- You can install the released version of ipa from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("ipa") -->

<!-- ``` -->

You can install the development version of `ipa` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bcjaeger/ipa")
```

# Example: diabetes data

These data are courtesy of Dr John Schorling, Department of Medicine,
University of Virginia School of Medicine. The data originally described
1046 participants who were interviewed in a study to understand the
prevalence of obesity, diabetes, and other cardiovascular risk factors
in central Virginia for African Americans. A total of 403 participants
were screened for diabetes, which was defined as glycosolated hemoglobin
\> 7.0. Among those 403 participants, 366 provided complete data for the
variables in these data.

First, we’ll load some packages and set a seed for reproducibility

``` r

library(tidymodels)
library(tidyverse)
library(ipa)

data("diabetes", package = 'ipa')

diab_complete <- diabetes$complete
diab_missing <- diabetes$missing

glimpse(diab_missing)
#> Observations: 366
#> Variables: 14
#> $ diabetes       <fct> No, No, No, No, Yes, No, No, No, No, No, No, No, No,...
#> $ chol           <int> NA, 165, NA, NA, 249, 248, 195, 177, NA, NA, 215, NA...
#> $ stable_glucose <int> 82, 97, NA, NA, 90, 94, NA, 87, NA, 82, 128, NA, 76,...
#> $ hdl            <int> NA, 24, NA, NA, 28, 69, NA, 49, 40, NA, NA, NA, 30, ...
#> $ age            <int> 46, 29, 58, 67, 64, 34, NA, 45, NA, NA, NA, NA, 36, ...
#> $ gender         <fct> NA, female, NA, NA, male, male, NA, male, NA, NA, NA...
#> $ frame          <fct> NA, large, NA, large, medium, large, NA, large, NA, ...
#> $ sbp            <int> 118, 112, NA, NA, 138, 132, 161, 160, NA, 130, NA, N...
#> $ dbp            <int> NA, 68, NA, NA, 80, 86, NA, 80, NA, NA, NA, 80, 66, ...
#> $ waist          <int> NA, 46, 49, 33, 44, 36, NA, 34, 45, NA, 42, NA, 36, ...
#> $ hip            <int> NA, 48, NA, NA, 41, 42, NA, 40, NA, NA, NA, NA, 40, ...
#> $ time_ppn       <int> NA, 360, 180, 480, 300, 195, 720, 300, 240, NA, NA, ...
#> $ height_cm      <dbl> NA, 1.6256, 1.5494, NA, 1.7272, 1.8034, 1.7526, 1.75...
#> $ weight_kg      <dbl> 54.88474, 98.88325, NA, NA, 83.00750, 86.18265, NA, ...
```

## K-nearest-neighbor imputation

K-nearest-neighbors (KNN) is a flexible and useful method for imputation
of missing data. Briefly, each missing value is imputed by aggregating
or randomly sampling a value from the K observations with greatest
similarity to the current obervation. Conventional methods for KNN
provide imputations using a single value of K, which makes it hard to
identify an optimal value of K. Ideally, one would generate imputed
datasets using a number of different values of K, and then use whichever
K provided the most accurate imputed values or the most accurate
prediction model (usually, these K are the same or similar). This is one
of the things `ipa` does.

Generally, the `ipa` workflow follows the same patterned steps. Here is
an example of this workflow using k-nearest-neighbors. A more detailed
explanation of the steps below is written in the `Getting started`
vignette.

``` r

nbrs_brew <- brew_nbrs(diab_missing, outcome = diabetes) %>%
  verbose_on(level = 1) %>% # makes the brew noisy
  spice(with = spicer_nbrs(k_neighbors = 1:35)) %>% # tuning parameters
  mash() %>% # creates imputed values 
  ferment() %>% # turns values into a data frame
  bottle(type = 'tibble') # turns data into tibbles, adds outcome column
#> Imputing chol, N observed = 227
#> Imputing stable_glucose, N observed = 226
#> Imputing hdl, N observed = 223
#> Imputing age, N observed = 231
#> Imputing gender, N observed = 235
#> Imputing frame, N observed = 227
#> Imputing sbp, N observed = 228
#> Imputing dbp, N observed = 227
#> Imputing waist, N observed = 211
#> Imputing hip, N observed = 223
#> Imputing time_ppn, N observed = 236
#> Imputing height_cm, N observed = 221
#> Imputing weight_kg, N observed = 243

nbrs_brew
#> A <U+0001F37A> to handle missing data using k-nearest-neighbors. 
#> # A tibble: 35 x 3
#>    impute pars             training           
#>     <int> <list>           <list>             
#>  1      1 <named list [1]> <tibble [366 x 14]>
#>  2      2 <named list [1]> <tibble [366 x 14]>
#>  3      3 <named list [1]> <tibble [366 x 14]>
#>  4      4 <named list [1]> <tibble [366 x 14]>
#>  5      5 <named list [1]> <tibble [366 x 14]>
#>  6      6 <named list [1]> <tibble [366 x 14]>
#>  7      7 <named list [1]> <tibble [366 x 14]>
#>  8      8 <named list [1]> <tibble [366 x 14]>
#>  9      9 <named list [1]> <tibble [366 x 14]>
#> 10     10 <named list [1]> <tibble [366 x 14]>
#> # ... with 25 more rows
```
