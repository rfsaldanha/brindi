get_sim <- function(agg, ano, sexo = NULL){

  # Variable aggregation name
  if(agg == "uf_res"){
    agg <- "res_SIGLA_UF"
  } else if (agg == "uf_ocor"){
    agg <- "ocor_SIGLA_UF"
  } else if (agg == "mun_res"){
    agg <- "res_codigo_adotado"
  } else if (agg == "mun_ocor"){
    agg <- "ocor_codigo_adotado"
  }

  # SQL query basic partials
  sql_select <- glue::glue("SELECT {agg} AS agg, COUNT(1) AS freq")
  sql_from <- glue::glue("FROM \"datasus-sim\"")
  sql_where <- glue::glue("WHERE ano_obito = {ano}")
  sql_group_by <- glue::glue("GROUP BY {agg}")

  # Adds to where partial
  if(!is.null(sexo)){
    sql_where <- glue::glue(sql_where, " AND def_sexo = '{sexo}'")
  }

  # Create SQL query string
  sql_query <- glue::glue(sql_select, sql_from, sql_where, sql_group_by, .sep = " ")

  # Create list with token and SQL query
  request_body <- list(token = list(token = pcdas_token), sql = list(sql = list(query = sql_query, fetch_size = 10000)))

  # Create request
  req <- httr2::request(base_url = pcdas_url) %>%
    httr2::req_url_path_append("sql_query") %>%
    httr2::req_body_raw(jsonlite::toJSON(request_body, auto_unbox = TRUE)) %>%
    httr2::req_throttle(10 / 60, realm = pcdas_url) %>%
    httr2::req_retry(max_tries = 3)

  # Perform request
  resp <- httr2::req_perform(req = req)

  # Get content
  content <- httr2::resp_body_json(resp)

  # Transform content to data.frame and tibble
  content_df <- convert_content_to_df(content) %>%
    tibble::as_tibble()

  return(content_df)
}
