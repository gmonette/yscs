#' Evaluate character strings as an expression
#'
#' Evaluate character strings as an expression in the appropriate frame so the function can be
#' used in a function and use variables in the environment of the function.
#'
#' @param ... expressions that can evaluate to character strings to paste. Note that the separator
#' for pasting is '' to allow expressions within quotes.
#' @param envir (default = parent.frame()) see \code{\link{eval}}.
#' @param enclos (default same as \code{\link{eval}})
#' @param try (default TRUE) enclose the expression in \code{\link{try}}.
#' @return the result of evaluating \code{paste(...,sep='')} as an expression
#' @examples
#' run('ls()')
#' for(nam in c('ls','search')) run(nam,"()")  # very unimaginative example
#' @export
run <- function(..., envir = parent.frame(),
                enclos = if(is.list(envir) || is.pairlist(envir)) parent.frame()
                else baseenv(),
                try = TRUE) {
  if (try) try(eval(parse(text = paste(...,sep='')), envir = envir, enclos = enclos))
  else eval(parse(text = paste(...,sep='')), envir = envir, enclos = enclos)
}
