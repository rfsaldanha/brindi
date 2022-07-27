#' Expand indicator results to SQLite
#'
#' @param agg character vector. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param anos numeric vector. Years
#' @param db character. SQLite file address and name.
#' @param indi character function names vector. Defaults to `all` for all indi_ functions.
#' @param table_name character. Indicators table name.
#'
#' @export
expand_indi_sqlite <- function(agg, anos, db, indi = "all", table_name = "indi"){

  # List indi_ functions or use supplied vector
  if(indi == "all"){
    indi_funs <- grep("^indi_", ls(getNamespace("bilis")), value = TRUE)
  } else {
    indi_funs <- c(indi)
  }

  # Creates database connection
  conn <- DBI::dbConnect(RSQLite::SQLite(), db)

  # Remove table if exists
  if(DBI::dbExistsTable(conn = conn, name = table_name)){
    DBI::dbRemoveTable(conn = conn, table_name)
  }

  # Expand indi_functions and write to table
  for(i in indi_funs){
    tmp <- expand_indi(agg = agg, anos = anos, indi_fun = i)
    DBI::dbWriteTable(conn = conn, name = table_name, value = tmp, append = TRUE)
  }

  # Close connection
  DBI::dbDisconnect(conn = conn)
}
