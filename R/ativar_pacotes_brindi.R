# ============================================================
# BRINDI | Preparar ambiente de trabalho
# Instala pacotes ausentes, carrega bibliotecas e configura
# o token de acesso a Plataforma de Ciencia de Dados em Saude.
# ATENCAO: este arquivo contem um token pessoal. Nao compartilhe
# nem publique em repositorios Git/GitHub.
# ============================================================

options(repos = c(CRAN = "https://cloud.r-project.org"))

# 1. Instalar somente os pacotes ausentes ----------------------
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

if (!requireNamespace("rpcdas", quietly = TRUE)) {
  remotes::install_github("rfsaldanha/rpcdas", upgrade = "never")
}

if (!requireNamespace("brpop", quietly = TRUE)) {
  install.packages("brpop")
}

if (!requireNamespace("brindi", quietly = TRUE)) {
  remotes::install_github(
    "rfsaldanha/brindi",
    dependencies = TRUE,
    upgrade = "never"
  )
}

# 2. Carregar pacotes -----------------------------------------
library(rpcdas)
library(brpop)
library(brindi)

# 3. Configurar o token PCDaS ---------------------------------
# Token pessoal informado pela usuaria: nao compartilhar.
Sys.setenv(
  pcdas_token = "20901bh3628342c5da896162c71g6f1c85a1559a"
)

if (nzchar(Sys.getenv("pcdas_token"))) {
  message("Token PCDaS configurado nesta sessao.")
} else {
  stop("Nao foi possivel configurar o token PCDaS.")
}

message("Ambiente BRINDI preparado: rpcdas, brpop e brindi carregados.")

# 4. Exemplos (execute separadamente) --------------------------
# brindi::indi_0001(agg = "mun_res", ano = 2022)
# Para a nova funcao MRT_4_000(), carregue antes o arquivo local:
# source("MRT_4_000.R")
# MRT_4_000(agg = "mun_res", ano = 2022)
