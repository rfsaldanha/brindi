get_pcdas_token_renviron <- function(){
  pcdas_token <- Sys.getenv("pcdas_token")
  if(pcdas_token == ""){
    stop("PCDaS token API not provided and not found on renvirom. Please provide PCDaS API token.")
  }

  return(pcdas_token)
}
