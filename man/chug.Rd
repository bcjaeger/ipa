% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chug.R
\name{chug}
\alias{chug}
\title{Chug a brew}
\usage{
chug(brew, .fun = NULL, .fun_args = NULL)
}
\arguments{
\item{brew}{an \code{ipa_brew} object.}

\item{.fun}{a function with at least three inputs: \code{.trn} \code{.tst},
and \code{outcome}. \code{scrimp_mdl()} will call your function as follows:
\code{.fun(.trn = train_imputed, .tst = test_imputed, outcome = outcome, ...)},
where \code{...} is filled in by \code{.fun_args}. Generally, \code{.fun} should
\enumerate{
\item develop a prediction model using \code{.trn}
\item create predicted values for observations in \code{.tst}
\item evaluates the predictions using a summary measure (e.g., R-squared,
area underneath the ROC curve, Brier score, etc.).
}

See example below where a function using random forests is applied.}

\item{.fun_args}{a named list with additional arguments for \code{.fun}.}
}
\value{
an \code{ipa_brew} object with a column added to the \code{wort}
containing the output from \link{scrimp_mdl}.
}
\description{
Chug a brew
}
