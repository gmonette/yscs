% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ellipse.R
\name{ellplus}
\alias{dellplus}
\alias{ell}
\alias{ellplus}
\title{Augmented ellipse}
\usage{
ellplus(center = rep(0, 2), shape = diag(2), radius = 1, n = 100,
  angles = (0:n) * 2 * pi/n, fac = chol, ellipse = all, diameters = all,
  box = all, all = FALSE)

dellplus(x, y, ...)

ell(center = rep(0, 2), shape = diag(2), radius = 1, n = 100)
}
\arguments{
\item{center}{(default: c(0,0))}

\item{shape}{(default: diag(2)) 'variance' corresponding to shape of ellipse
if taken as a concentration ellipse for a bivariate normal}

\item{radius}{(default: 1)}

\item{n}{(default: 100) number of points generated}

\item{angles}{(default (0:n)*2*pi/n) at which points are generated. The angles
refer to the generating circle that is then transformed linearly to
form the ellipse}

\item{fac}{(default: chol) method use to factor 'shape'}

\item{ellipse}{Logical to indicate if the points on the ellipse should be
returned}

\item{diameters}{Logical to indicate if the points defining the ends of the
conjugate axes of the ellipse should be returned}

\item{box}{Logical to indicate if the points on the conjugate-axes bounding
box should be returned}

\item{all}{Logical to request all of \code{ellipse}, \code{diameters} and
\code{box}. If \code{FALSE}, only the components specified separately by
\code{ellipse}, \code{diameters} and \code{box} are returned.}

\item{x, y}{data values}

\item{\dots}{arguments passed to \code{ellplus}}

\item{center}{X,Y location of the center of the ellipse}

\item{shape}{A 2x2 matrix, typically a covariance matrix of data (for a data
ellipse), or a covariance matrix of estimated parameters in a model (for a
confidence ellipse).}

\item{radius}{Radius of the ellipse-generating unit circle.  The default,
\code{radius=1} corresponds to a "standard" ellipse.}

\item{n}{Number of points on the unit circle used to calculate the ellipse}

\item{angles}{Angles around the unit circle used to calculate the ellipse}

\item{fac}{A function defining the conjugate axes used to transform the unit
circle into an ellipse.  The default, \code{\link{chol}}, uses the right Cholesky
factor of \code{shape}.}
}
\value{
Returns a 2-column matrix of (X,Y) coordinates suitable for drawing
with \code{lines()}.

For \code{ellplus}, when more than one of the options \code{ellipse},
\code{diameters}, and \code{box} is \code{TRUE}, the different parts are
separated by a row of \code{NA}.
}
\description{
Generates an ellipse augmented with conjugate axes and tangents

\code{ell} is a utility function used to calculate the (X, Y) coordinates of
a 2D ellipse for the purpose of drawing statistical diagrams and plots.
}
\details{
ellplus can produce, in addition to the points of an ellipse, the
conjugate axes corresponding to a chol or other decomposition
and the surrounding parallelogram.

\code{\link{ellplus}} can produce, in addition, the
conjugate axes corresponding to a \code{\link{chol}} or other decomposition and the
surrounding parallelogram defined by these axes.

Add function that works with direction to compute slices and conjugate directions.
}
\section{Functions}{
\itemize{
\item \code{ellplus}: tangents and axes

\item \code{dellplus}: data ellipse with tangents and axes
}}
\examples{
plot( x=0,y=0, xlim = c(-3,3), ylim = c(-3,3),
      xlab = '', ylab = '', type = 'n', asp=1)
abline( v=0, col="gray")
abline( h=0, col="gray")
A <- cbind( c(1,2), c(1.5,1))
W <- A \%*\% t(A)

lines( ell(center=c(0,0), shape = W ), col = 'blue', lwd=3)
lines( ellplus(center=c(0,0), shape = W, box=TRUE, diameters=TRUE ), col = 'red')

# show conjugate axes for PCA factorization
pca.fac <- function(x) {
    xx <- svd(x)
    ret <- t(xx$v) * sqrt(pmax( xx$d,0))
    ret
}

plot( x=0,y=0, xlim = c(-3,3), ylim = c(-3,3),
      xlab = '', ylab = '', type = 'n', asp=1)
abline( v=0, col="gray")
abline( h=0, col="gray")
lines( ell(center=c(0,0), shape = W ), col = 'blue', lwd=3)
lines( ellplus(center=c(0,0), shape = W, box=TRUE, diameters=TRUE, fac=pca.fac ), col = 'red')
}
\author{
Georges Monette
}
\seealso{
\code{\link{cell}}, \code{\link{dell}}, \code{\link{dellplus}},
}
\keyword{aplot}
\keyword{dplot}

