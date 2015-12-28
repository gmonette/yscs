#' Create a long file from a wide file
#'
#' Uses a minimal number  of arguments to create a long file using \code{stats::\link{reshape}}. Produces output even when long variable names and time values are not fully crossed.
#'
#' \code{long} is intended for the simple case in which 'wide' variables in the input data frame are identified
#' by the fact that they contain a separator character that separates the name of the variable in the long file from
#' the value of the 'time' variable that identifies the corresponding row in the long file, e.g \code{x_1, x_2, x_3} or
#' \code{brain.volume_left, brain.volume_right}.  If the separator ('_' by default) occurs in other variables, it must
#' be temporarily substituted.
#'
#' \code{\link{rehape}} does not work if long variable names and time values are not fully crossed, e.g \code{x_1, x_2, x_3, y_1, y_2}. By default \code{long}
#' creates additional variables with "NAs" so the set of variables given to \code{\link{reshape}} is fully crossed, e.g. adding a variable \code{y_3 <- NA}.
#'
#' \code{\link{to_long}} is a synonym for compatibility with the 'spida' package.
#'
#' @param data wide data frame
#' @param sep (default '_') single character separator between long names and 'time' value.
#'        Variable names with this separator are transformed to long variables.
#' @param timevar (default 'time') names of variable in output long file to identify occasions. Its values are taken from the suffix following
#'        the 'sep' character in each time-varying variable.
#' @param idvar  (default: 'id') the variable name used in the output long file to identify rows in the input wide file. It may exist in the input wide file and must, in that case,
#'        have a unique value in each row. If it does not exist, it will be created with values equal to the row numbers of the input wide file.
#' @param ids  (default \code{1:nrow(data)}) values for idvar in long file if the variable \code{idvar} does not exist in in the input wide file. Ignored
#'        if \code{idvar} exists in \code{data}.
#' @param expand (default TRUE): if 'time' values are inconsistent, fill in missing 'time's with NAs.
#' @param safe_sep temporary safe? separator
#' @param ... additional parameters are passed to \code{\link{reshape}}.
#' @return 'long' file with each wide row repeated as many times as there are distinct values for the 'timevar' variable.
#' @examples
#' z <- data.frame(id =letters[1:10], id2= 11:20, v_L = 1:10, v_R = 11:20)
#' long(z)
#' long(z, timevar = 'Side', idvar = 'idn', ids = LETTERS[1:10])
#' long(z, timevar = 'Side', idvar = 'idn', ids = z$id2)
#'
#' # unbalanced times
#' z <- data.frame(id =letters[1:10], id2= 11:20, v_L = 1:10, v_R = 11:20, z_L = 21:30)
#' long(z)
#'
#' # multi-character sep
#' z <- data.frame(id =letters[1:10], id2= 11:20, HPC_head_R = 1:10, HPC_tail_R = 11:20, HPC_head_L = 21:30, HPC_tail_L = 31:40)
#' names(z) <- sub("(_[LR]$)","_\\1", names(z))
#' names(z)
#' (zz <- long(z, sep = "__", timevar = "Side"))
#' zz$id3 <- rownames(zz)
#' long(zz, idvar = 'id3' ,timevar = 'Part')
#'
#' @export
long <- function (data, sep = "_",  timevar = 'time',
                  idvar = 'id', ids = 1:nrow(data),
                  expand = TRUE, safe_sep = "#%@!", ...) {
  if (timevar %in% names(data)) warning(paste("Variable",timevar, "in data is replaced by a variable to mark occasions"))
  if (idvar %in% names(data)) {
    idwide <- data[[idvar]]
    if( length(unique(idwide)) != length(idwide)) {
      warning(paste ("idvar:", idvar, "must have unique values. It will be replaced in the output data frame with a variable containing row numbers of the input data frame"))
      data[[idvar]] <- ids
    }
  }
  if (expand) {
    namessafe <- sub(sep, safe_sep, names(data), fixed = TRUE)
    varnames <- grep(safe_sep, namessafe, value = TRUE, fixed = TRUE)
    names <- unique(sub(paste(safe_sep, ".*$", sep = ""),
                        "", varnames))
    times <- unique(sub(paste("^.*", safe_sep, sep = ""),
                        "", varnames))
    allnames <- paste(rep(names, each = length(times)), sep,
                      rep(times, length(names)), sep = "")
    z <- data
    for (nn in allnames) {
      z[[nn]] <- if (is.null(data[[nn]]))
        NA
      else data[[nn]]
    }
    data <- z
  }
  namessafe <- sub(sep, safe_sep, names(data), fixed = TRUE)
  namestimes <- sub(paste("^.*", safe_sep, sep = ""), "ZZZZZ",
                    namessafe)
  ord <- order(namestimes)
  data <- data[, ord]
  stats::reshape(data, direction = "long", sep = sep,
          varying = grep(sep, names(data), fixed = TRUE),
          timevar = timevar, idvar = idvar,
          ids = ids, ...)
}
#' @export
#' @rdname long
to_long <- long
#  z <- data.frame(id =letters[1:10], id2= 11:20, v_L = 1:10, v_R = 11:20)
#  long(z)
#  long(z, timevar = 'Side', idvar = 'idn', ids = LETTERS[1:10])
#  long(z, timevar = 'Side', idvar = 'idn', ids = z$id2)
# z$nid <- 1:5
# long(z, idvar = 'nid')
# z
#  # unbalanced times
#  z <- data.frame(id =letters[1:10], id2= 11:20, v_L = 1:10, v_R = 11:20, z_L = 21:30)
#  long(z,idvar='rowid')
#
#  reshape(direction= 'long', z, sep = '_', v.names= 'v') # does not work
