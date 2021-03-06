% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pfmt.R
\name{pfmt}
\alias{pfmt}
\title{Displaying p-values}
\usage{
pfmt(x, digits = 5, scientific = FALSE)
}
\arguments{
\item{x}{a n x 2 matrix or data frame}

\item{digits}{(default: 5) number of digits for rounding coefficient}

\item{pdigits}{(default: 5) number of digits for rounding p-values}
}
\description{
Displaying p-values
}
\examples{
rpfmt( cbind( 'estimate' = rnorm(7), "p-values" = 10^c(-1,-2,-3,-4,-5,-6,-7)))
}

