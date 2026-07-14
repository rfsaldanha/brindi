#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

utils::globalVariables(c(
  "adj.rate", "age_group", "cod", "cod_reg_saude", "code_muni", "codi_reg_saude",
  "count", "crude.rate", "events", "freq", "lci", "pop",
  "population", "uci", "uf", "year"
))
