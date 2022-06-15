#' List PCDaS tables
#'
#' List PCDaS indexes available through API token used.
#'
#' @return A vector.
#'
#' @export
list_pcdas_tables <- function(){
  pcdas_token <- Sys.getenv("pcdas_token")

  req <- httr2::request(base_url = pcdas_url) %>%
    httr2::req_url_path_append("show_tables") %>%
    httr2::req_body_json(data = list("token" = pcdas_token)) %>%
    httr2::req_throttle(10 / 60, realm = pcdas_url) %>%
    httr2::req_retry(max_tries = 3)

  resp <- httr2::req_perform(req = req)

  content <- unlist(httr2::resp_body_json(resp)$databases)

  return(content)
}
