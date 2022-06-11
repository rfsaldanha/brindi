# Function to create pcdas token
set_pcdas_token <- function(){
  keyring::key_set(service = "pcdas", prompt = "Please inform your PCDaS token.")
}

# Prompt user to set PCDaS token if it does not exist
if(!("pcdas" %in% keyring::key_list()$service)){
  set_pcdas_token()
}

# Create environment variable with token
pcdas_token <- keyring::key_get(service = "pcdas")
