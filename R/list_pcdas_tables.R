#' List PCDaS tables
#'
#' List PCDaS indexes available through API token used.
#'
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#'
#' @return A vector.
#'
#' @export
list_pcdas_tables <- function(pcdas_token = NULL){
  # Function argument check
  checkmate::assert_string(x = pcdas_token, null.ok = TRUE)

  # Try to get PCDaS API token from renviron of not provided
  if(is.null(pcdas_token)){
    pcdas_token <- get_pcdas_token_renviron()
  }

  # Create requisition
  req <- httr2::request(base_url = pcdas_url) %>%
    httr2::req_url_path_append("show_tables") %>%
    httr2::req_body_json(data = list("token" = pcdas_token)) %>%
    httr2::req_throttle(30 / 60, realm = pcdas_url) %>%
    httr2::req_retry(max_tries = 3)

  # Perform requisition
  resp <- httr2::req_perform(req = req)

  # Retrieve content
  content <- unlist(httr2::resp_body_json(resp)$databases)

  return(content)
}
