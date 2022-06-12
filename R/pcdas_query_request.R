pcdas_query_request <- function(body, throttle_rate = 10/60, max_tries = 3){
  # Create request
  req <- httr2::request(base_url = pcdas_url) %>%
    httr2::req_url_path_append("sql_query") %>%
    httr2::req_body_raw(body) %>%
    httr2::req_throttle(throttle_rate, realm = pcdas_url) %>%
    httr2::req_retry(max_tries = max_tries)

  # Perform request
  resp <- httr2::req_perform(req = req)

  # Get content
  content <- httr2::resp_body_json(resp)
}
