do_call_indi <- function(indi, agg, agg_time, ano, save_args, p, engine, psql_args){
  # Progress bar update
  p()

  # Call indicator function with arguments
  res <- do.call(what = indi, args = list(agg = agg, agg_time = agg_time, ano = ano, save_args = save_args))

  # Save result
  if(engine == "psql"){
    # Try to get psql user from renviron if not provided
    if(is.null(psql_args$psql_user)){
      psql_user <- get_psql_user()
    } else{
      psql_user <- psql_user
    }

    # Try to get psql password from renviron if not provided
    if(is.null(psql_args$psql_pwd)){
      psql_pwd <- get_psql_pwd()
    } else {
      psql_pwd <- psql_pwd
    }

    # Creates database connection
    conn <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = psql_args$psql_db,
      host = psql_args$psql_host,
      port = psql_args$psql_port,
      user = psql_user,
      password = psql_pwd
    )

    # Close connection
    on.exit(DBI::dbDisconnect(conn = conn))

    # Remove table if exists
    if(DBI::dbExistsTable(
      conn = conn,
      DBI::Id(schema = psql_args$psql_schema, table = psql_args$psql_table)
    )){
      DBI::dbRemoveTable(
        conn = conn,
        name = DBI::Id(schema = psql_args$psql_schema, table = psql_args$psql_table)
      )
    }

    # Write results
    DBI::dbWriteTable(
      conn = conn,
      name = DBI::Id(schema = psql_args$psql_schema, table = psql_args$psql_table),
      value = res,
      append = TRUE
    )


  }

  return(TRUE)
}
