#' Get psql user name and password from renviron
#'
#' @return character.
#'
get_psql_user <- function(){
  psql_user <- Sys.getenv("psql_user")
  if (psql_user == "") {
    stop("psql user not provided and not found on renviron. Please provide psql user")
  }
  return(psql_user)
}

get_psql_pwd <- function(){
  psql_pwd <- Sys.getenv("psql_pwd")
  if (psql_pwd == "") {
    stop("psql password not provided and not found on renviron. Please provide psql user")
  }
  return(psql_pwd)
}
