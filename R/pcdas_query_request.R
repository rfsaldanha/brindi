#' Execute PCDaS query request
#'
#' Execute PCDaS API query request.
#'
#' @param body Body of the request, in JSON format.
#' @param throttle_rate Rate of requests per second allowed. Defaults to 30/60 (thirty requests per minute).
#' @param max_tries Max number of retries before fail. Defaults to 3.
#'
#' @return A list
#'
pcdas_query_request <- function(body, throttle_rate = 30, max_tries = 3){
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
