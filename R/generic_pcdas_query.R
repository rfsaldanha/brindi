#' Executes a generic PCDaS query
#'
#' Executes a generic PCDaS API query.
#'
#' @param sql_query character. SQL.
#'
#' @return A tibble
#'
#' @details
#' The query is limited to retrieve 100 variables and 10,000 records by the ElasticSearch cluster.
#'
#' In the query, must be a usea a database listed with \code{list_pcdas_tables}.
#'
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#'
#' @examples
#' # Runs a query
#' generic_pcdas_query(sql_query = "SELECT N_AIH, DT_INTER, PROC_REA FROM \"datasus-sih\" LIMIT 100")
#' @export

generic_pcdas_query <- function(pcdas_token = NULL, sql_query){
  # Function argument check
  checkmate::assert_string(x = pcdas_token, null.ok = TRUE)
  checkmate::assert_string(x = sql_query)

  # Try to get PCDaS API token from renviron of not provided
  if(is.null(pcdas_token)){
    pcdas_token <- get_pcdas_token_renviron()
  }

  # Create list with token and SQL query
  request_body <- list(token = list(token = pcdas_token), sql = list(sql = list(query = sql_query, fetch_size = 10000)))

  # Request body as JSON
  request_body_json <- jsonlite::toJSON(request_body, auto_unbox = TRUE)

  # Execute PCDaS API request
  content <- pcdas_query_request(body = request_body_json)

  # Transform content to data.frame and tibble
  content_df <- convert_content_to_df(content) %>%
    tibble::as_tibble()

  # Return
  return(content_df)
}
