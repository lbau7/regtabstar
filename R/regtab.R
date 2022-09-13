#' Regression Tables
#'
#' regtab is a generic function that creates tables for the results of
#' various regression models.
#'
#' @param mod A model object for which a table should created.
#' @template dotdotdot
#'
#' @return \code{regtab} uses \code{kableExtra::kbl} to return a table.
#' @export
regtab <- function(mod, ...) {
  UseMethod("regtab", mod)
}
