#' Wide data set from long data set
#'
#' Creates a wide data frame from a long data frame identifying
#' an 'id' variable and using a time variable whose values
#' provide suffixes for time-varying variable names in wide form.
#'
#' In contrast with reshape in stats, this function
#' identifies variables that
#' are invariant with respect to 'idvar' and does not
#' expand them to wide form.
#'
#' Only 'time'-varying variables are expanded to wide form.
#'
#' @param data a data frame in 'long' form.
#' @param idvar (default: 'id') the variable identifying each group of rows
#' that are transformed to a single row in the wide file. Should be a single variable if the default for \code{invar} is accepted.
#' @param timevar (default: 'time') the variable containing
#' the occasion names in the long file.
#' @param sep (default: '_') the character(s) that separate the name of
#' a time-varying variable in the long form
#' from the added suffix for the
#' correponding names in wide form. Default: '_'.
#' @param invar additional variables treated as invariant
#' within clusters
#' (default: names(data)[gicc(data, data[idvar])]).
#' For all variables, except \code{idvar} to be treated as time-varying,
#' use \code{invar = NULL}.
#' @param \dots Other arguments are passed to \link{list("stats::reshape")}.
#' @return a data frame in wide form in which each variable that varies
#' within levels of 'idvar' is turned into as many variables as there are
#' distinct values of 'timevar' using the values of 'timevar' as suffixes to
#' name the variables in wide form.
#' @examples
#' \dontrun{
#' dd <- data.frame( y.a = 1:3, y.b = 1:3, x.a= 1:3, time = 1:3,
#'                   x.b = 11:13, x.c = 21:23, id = c('a','a','b'))
#' tolong(dd, sep = '.')
#' dl <- tolong(dd, sep = '.', timevar = "type", idvar = 'patient')
#' towide(dl, idvar = 'patient', timevar = 'type')
#'
#' # Long file with additional constants
#'
#' dl <- data.frame(name = rep(c('A','B','C'), c(3,3,2)),
#'                  site = c('head','neck','jaw','chest')[
#'                       c(1,2,3,1,2,3,1,4)],
#'                  sex = rep(c('male','female','male'), c(3,3,2)),
#'                  var1 = 1:8,
#'                  var2 = 11:18,
#'                  invar = rep(1:3, c(3,3,2)))
#' towide(dl, c('name','sex'), 'site')
#'
#' # Two indexing variable: e.g. hippocampal volume: 2 sides x 3 sites
#'
#' dl <- data.frame(name = rep(LETTERS[1:3], each = 6),
#'                  side = rep(c('left','right'), 9),
#'                  site = rep(rep(c('head','body','tail'),each = 2),3),
#'                  volume = 1:18,
#'                  sex = rep(c('female','male','female'), each = 6),
#'                  age = rep(c(25, 43, 69), each = 6))
#' dl
#' (dl.site <- towide(dl, c('name','side'), 'site'))
#' (dl.site.side <- towide(dl.site, c('name'), 'side'))
#' }
#' @export
towide <- function(data,
                   idvar = 'id',
                   timevar = 'time',
                   sep = '_',
                   add.invariants = TRUE,
                   ...) {
  invars <- function( data, idvar) {
    # identify invariant variables
    ret <- sapply(data, function(x) {
      max(capply(x,data[,idvar], function(y) length(unique(y))))
    }
    )
    ret <- ret == 1
    ret
  }
  if(add.invariants) idvar <- names(data)[invars(data, idvar)]
  # disp(idvar)
  # disp(timevar)
  stats::reshape(data, direction = 'wide',
                 idvar = idvar,
                 timevar = timevar, sep = sep, ...)
}
