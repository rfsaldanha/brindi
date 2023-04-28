#' Expand several indicators results to PostgreSQL
#'
#' @param agg character vector. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param agg_time character vector. Time aggregation levels. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param anos numeric vector. Years
#' @param indi character function names vector. Defaults to `all` for all indi_ functions.
#' @param psql_table character. Indicators table name.
#' @param psql_schema character. Schema name.
#' @param psql_db character. Database name.
#' @param psql_host character. psql server host address.
#' @param psql_port character. psql server port.
#' @param psql_user character. psql user name. If not provided, the function will look for it on renviron.
#' @param psql_pwd character. psql password. If not provided, the function will look for it on renviron.
#'
#' @export
expand_indi_psql <- function(agg, agg_time, anos, indi = "all", psql_table, psql_schema, psql_db, psql_host, psql_port, psql_user = NULL, psql_pwd = NULL){

  # Try to get psql user from renviron if not provided
  if(is.null(psql_user)){
    psql_user <- get_psql_user()
  }

  # Try to get psql password from renviron if not provided
  if(is.null(psql_pwd)){
    psql_pwd <- get_psql_pwd()
  }

  # List indi_ functions or use supplied vector
  if(length(indi) == 1){
    if(indi == "all"){
      indi_funs <- grep("^indi_", ls(getNamespace("bilis")), value = TRUE)
    } else {
      indi_funs <- c(indi)
    }
  } else {
    indi_funs <- c(indi)
  }

  # Creates database connection
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = psql_db,
    host = psql_host,
    port = psql_port,
    user = psql_user,
    password = psql_pwd
  )

  # Close connection
  on.exit(DBI::dbDisconnect(conn = conn))

  # Remove table if exists
  if(DBI::dbExistsTable(
    conn = conn,
    DBI::Id(schema = psql_schema, table = psql_table)
  )){
    DBI::dbRemoveTable(
      conn = conn,
      name = DBI::Id(schema = psql_schema, table = psql_table)
    )
  }

  # Creates progress bar
  pb <- progress::progress_bar$new(
    format = ":current/:total :what [:bar] :percent in :elapsed ETA: :eta",
    clear = FALSE, total = length(indi_funs))

  pb$tick(0)
  Sys.sleep(1)

  # Expand indi_functions and write to table
  for(i in indi_funs){
    pb$tick(tokens = list(what = i))
    tmp <- expand_indi(agg = agg, agg_time = agg_time, anos = anos, indi_fun = i)
    DBI::dbWriteTable(
      conn = conn,
      name = DBI::Id(schema = psql_schema, table = psql_table),
      value = tmp,
      append = TRUE
    )
  }
}
