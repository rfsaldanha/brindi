## code to prepare `mun_pop` dataset goes here

# Anos
anos <- 1998:2019

# Empty tibble
mun_pop <- tibble::tibble()

for(ano in anos){
  # Query
  sql_query <- glue::glue("SELECT * FROM \"datasus-pop\" WHERE ANO = {ano}")

  # Create list with token and SQL query
  request_body <- list(token = list(token = pcdas_token), sql = list(sql = list(query = sql_query, fetch_size = 10000)))

  # Request body as JSON
  request_body_json <- jsonlite::toJSON(request_body, auto_unbox = TRUE)

  # Execute PCDaS API request
  content <- pcdas_query_request(body = request_body_json)

  # Transform content to data.frame and tibble
  content_df <- convert_content_to_df(content) %>%
    tibble::as_tibble()

  # Bind rows
  mun_pop <- dplyr::bind_rows(mun_pop, content_df)
}

usethis::use_data(mun_pop, overwrite = TRUE)
