#' Round and format
#'
#' Round to a given number of significant digits for variability
#'
#' Instead of rounding to a given number of significant digits, or rounding
#' to a given significance, 'rnd' rounds so that the variability in a set of
#' number is exhibited to a given number of digits.
#'
#' @param x an object to be rounded and formatted
#' @param digits number of digits for rounding
#' @param ... passed on to \code{\link{format}} function
#' @seealso \code{\link{pfmt}}, \code{\link{rpfmt}}
#' @examples
#' rnd(c(0.00001111111,0.000022222,0.000015), scientific = F)
#' rnd(c(0.00001111111,0.000022222,0.000015)+100, scientific = F)
#' round(c(0.00001111111,0.000022222,0.000015))
#' round(c(0.00001111111,0.000022222,0.000015)+100)
#' rnd(123)
#' rnd(123.45678901)
#' @export
rnd <- function(x, digits, ...) UseMethod('rnd')
#' @rdname rnd for numeric input
#' @export
#' @param digits (default=3) significant digits for variation in x
rnd.default <- function(x, digits = 3, ..., verbose = 0) {
  if (is.numeric(x)){
    ran <- diff(range(x, na.rm = T))
    mea <- mean(x, na.rm = T)
    # show variability
    dig <- max(round(-log(ran, base = 10) + digits),0)
    if(verbose) disp(dig)
    if(is.finite(dig)) {
      x <- round(x, digits = dig)
      ret <- format(x, nsmall = dig, ...)
    } else {
      ret <- format(signif(x, digits = digits + 3),...)
    }
  } else {
    ret <- format(x, ...)
  }
  ret
}

