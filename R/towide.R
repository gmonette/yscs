#' Wide data set from long data set
#'
#' Creates a wide data set from a long data set identifying an 'id' variable
#' and using a time variable whose values provide suffixes for time-varying
#' variable names in wide form.
#'
#' In contrast with reshape in stats, this function identifies variables that
#' are invariant with respect to 'idvar' and does not expand them to wide form.
#' Only 'time'-varying variables are expanded to wide form.
#'
#' @param data a data frame in 'long' form.
#' @param sep (default: '_') the character(s) that separate the name of the
#' a time-varying variable in the long form from the added suffix for the
#' correponding names in wide form. Default: '_'.
#' @param timevar (default: 'time') the variable containing the occasion names
#' in the long file.
#' @param idvar (default: 'id') the variable identifying each group of rows
#' that are transformed to a single row in the wide file.
#' @param ... Other arguments are passed to \link{list("stats::reshape")}.
#' @return a data frame in wide form in which each variable that varies
#' within levels of 'idvar' is turned into as many variables as there are
#' distinct values of 'timevar' using the values of 'timevar' as suffixes to
#' name the variables in wide form.
#' @examples
#'
#' \dontrun{
#' dd <- data.frame( y.a = 1:3, y.b = 1:3, x.a= 1:3, time = 1:3,
#'                   x.b = 11:13, x.c = 21:23, id = c('a','a','b'))
#' tolong(dd, sep = '.')
#' dl <- tolong(dd, sep = '.', timevar = "type", idvar = 'patient')
#' towide(dl, idvar = 'patient', timevar = 'type')
#' }
#'
#' @export
towide <- function(data, idvar = 'id', timevar = 'time', sep = '_') {
  ids  <- names(data)[gicc(data, data[[idvar]])==1]
  reshape(data, direction = 'wide', idvar = ids, timevar = timevar, sep = sep)
}
