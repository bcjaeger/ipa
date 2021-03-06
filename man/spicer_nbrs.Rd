% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spice.R
\name{spicer_nbrs}
\alias{spicer_nbrs}
\title{Neighbor's spices}
\usage{
spicer_nbrs(k_neighbors = seq(10), aggregate = TRUE)
}
\arguments{
\item{k_neighbors}{a numeric vector indicating how many neighbors should
be used to impute missing values.}

\item{aggregate}{a logical value. If \code{TRUE}, then neighbors
will be aggregated to generate imputations. If \code{FALSE}, then one
neighbor will be sampled at random to generate a missing value. Using
\code{aggregate = FALSE} can be helpful if you are conducting
multiple imputation.}
}
\value{
a list with input values that can be passed directly into
\link{spice}, e.g \code{spice(brew, with = spicer_nbrs())} for a neighbors
brew or \code{spice(brew, with = spicer_soft())} for a soft brew.
}
\description{
If you use Rstudio, the \code{masher} and \code{spicer} functions can help
remind you which parameters go along with which \code{ipa_brew} flavor.
The basic idea is to write \verb{spice(brew, with = spicer_<flavor>())}
and \verb{mash(brew, with = masher_<flavor>())}. Hitting the tab key with
your curser inside the parentheses of \code{masher_flavor()}will create a
drop-down menu that shows a list of the arguments that go along with
your brew's flavor.

If you have no trouble remembering the parameters that go along
with your brew's flavor, or if you just want your code to be more concise,
you don't have to use the \code{with} argument. Instead, you can just
specify parameter values directly using the \code{...} argument in the \code{mash}
and \code{spice} functions. In the examples below, both approaches are shown.
}
\examples{

data <- data.frame(
  x1 = 1:10,
  x2 = 10:1,
  x3 = rnorm(10),
  outcome = 11 + runif(10)
)

data[1:2, 1:2] = NA

knn_brew <- brew(data, outcome = outcome, flavor = 'kneighbors')
sft_brew <- brew(data, outcome = outcome, flavor = 'softImpute')

# these two calls are equivalent
spicy_knn <- spice(knn_brew, with = spicer_nbrs(k_neighbors = c(3, 5)))
spicy_knn <- spice(knn_brew, k_neighbors = c(3, 5))

# these two calls are equivalent
spicy_sft <- spice(sft_brew, with = spicer_soft(grid = TRUE))
spicy_sft <- spice(sft_brew, grid = TRUE)


}
