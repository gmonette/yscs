% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/up.R
\name{up}
\alias{up}
\title{Create a data frame at a higher level of aggregation}
\usage{
up(object, form = formula(object), all = FALSE, sep = "/",
  FUN = function(x) mean(x, na.rm = TRUE), omitGroupingFactor = FALSE,
  groups, invariantsOnly = !all, ...)
}
\arguments{
\item{object}{a data frame to be summarized.}

\item{form}{a one-sided formula identifying the variable(s) in \code{object}
that identifies clusters. e.g. ~ school/Sex to get a summary within each Sex
of each school.}

\item{all}{if TRUE, include summaries of variables that vary within
clusters, otherwise keep only cluster-invariant variables.}

\item{sep}{separator to form cluster names combining more than one
clustering variables.  If the separator leads to the same name for distinct
clusters (e.g. if var1 has levels 'a' and 'a/b' and var2 has levels 'b/c'
and 'c') the function produces an error and a different separator should be
used.}

\item{FUN}{function to be used for summaries.}

\item{omitGroupingFactor}{kept for compatibility with \code{gsummary}}

\item{groups}{kept for compatibility with \code{gsummary}}

\item{invariantsOnly}{kept for compatibility with \code{gsummary}}

\item{\dots}{additional arguments to \code{tapply} when summarizing
numerical variables. e.g. \code{na.rm = TRUE}}
}
\value{
a data frame with one row per value of the variable in \code{form}
}
\description{
Produce a higher level data set with one row per cluster. The data set can
contain only variables that are invariant in each cluster or it can also
include summaries (mean or modes) of variables that vary by cluster. Adapted
from \code{gsummary} in the \code{nlme} package.
}
\details{
\code{up} was created from \code{nlme::gsummary} and modified to make it
easier to use and to make an equivalent of \code{gsummary} available when
using \code{lme4}.
}
\examples{
    data(hs)
    dim( hs )
    hsu <- up( hs, ~ school )
    dim( hsu )

    # to also get cluster means of cluster-varying numeric variables and modes of factors:

    hsa <- up( hs, ~ school , all = TRUE )

    # to get summary proportions of cluster varying factors:

    up( cbind( hs, model.matrix( ~ Sex -1 , hs)), ~ school, all = T)


    ## To plot a summary between-cluster panel along with within-cluster panels:

    hsu <- up( hs, ~ school, all = TRUE)
    hsu$school <- ' between'  # space to make it come lexicographically before cluster names

    require( lattice )
    xyplot( mathach ~ ses | school, rbind(hs,hsu),
        panel = function( x, y, ...) {
            panel.xyplot( x, y, ...)
            panel.lmline( x, y, ...)
        } )

}
\author{
largely from gsummary in Bates & Pinheiro
}

