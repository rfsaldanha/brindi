# Prompt user to set PCDaS token if it does not exist
if(!("pcdas" %in% keyring::key_list()$service)){
  set_pcdas_token()
}

# Create environment variable with token
pcdas_token <- keyring::key_get(service = "pcdas")
