
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Imputation for predictive analytics (`ipa`)

<!-- Think about statistics in medicine submission -->

The goal of `ipa` is to make imputation in predictive modeling workflows
more straightforward and efficient. The main functions in `ipa` are

1.  `brew` Create a container to hold your imputations

2.  `spice` (optional) set parameters that govern the number of
    imputations for the given brew

3.  `mash` fit models that will provide imputations

4.  `ferment` impute missing values in training and (optionally) testing
    data

5.  `bottle` output the imputed datasets in a
tibble.

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

## Example

To be filled in
