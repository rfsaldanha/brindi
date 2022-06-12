# Function to create pcdas token
set_pcdas_token <- function(){
  keyring::key_set(service = "pcdas", prompt = "Please inform your PCDaS token.")
}
