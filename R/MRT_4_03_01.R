#' MRT.4.03.01 | Taxa de mortalidade por acidentes de transporte terrestre
#'
#' @description
#' Categoria: Mortalidade | Dimensão 4: Mortalidade por causas externas.
#' Estima os óbitos por acidentes de transporte terrestre (CID-10:
#' V01–V89) por 100.000 habitantes, com dados do SIM e população.
#' Permite análises por município, região de saúde e UF, segundo
#' residência ou ocorrência, com taxas brutas ou ajustadas por idade.
#'
#' @details
#' Fonte: SIM/DATASUS (PCDaS) | População: pacote brpop.
#' Cálculo: (óbitos / população) * 100.000.
#' Referência temática RIPSA: MRT.4.03 (lesões de trânsito).
#' Nota: acidentes de transporte terrestre (V01–V89) e lesões de
#' trânsito não são definições necessariamente equivalentes.
#' Na agregação por ocorrência, o denominador usa a população residente
#' do território, exigindo cautela na interpretação.
#'
#' @param agg Agregação geográfica: `mun_res` (município de residência),
#'   `mun_ocor` (município de ocorrência), `regsaude_res` (região de saúde de
#'   residência), `regsaude_ocor` (região de saúde de ocorrência),
#'   `regsaude_449_res` (região de saúde, 449 unidades, por residência),
#'   `regsaude_449_ocor` (região de saúde, 449 unidades, por ocorrência),
#'   `uf_res` (UF de residência) ou `uf_ocor` (UF de ocorrência).
#' @param agg_time Agregação temporal: `year` (ano), `month` (mês) ou
#'   `week` (semana). Padrão: `year`.
#' @param ano Ano de referência dos óbitos.
#' @param multi Multiplicador da taxa. Padrão: `100000`.
#' @param decimals Número de casas decimais. Padrão: `2`.
#' @param pop_source Fonte populacional disponível em brpop. Padrão: `datasus`.
#' @param adjust_rates Se `TRUE`, calcula a taxa ajustada por idade; se
#'   `FALSE`, calcula a taxa bruta. Padrão: `FALSE`.
#' @param pcdas_token Token da API PCDaS. Se `NULL`, é obtido do ambiente R.
#'
#' @return Resultado do indicador conforme as funções internas do BRINDI.
#'
#' @examples
#' \dontrun{
#' MRT_4_03_01(agg = "mun_res", ano = 2020)
#' MRT_4_03_01(agg = "regsaude_ocor", ano = 2020)
#' MRT_4_03_01(agg = "uf_res", ano = 2020, adjust_rates = TRUE)
#' }
#'
#' @importFrom rlang .data
#' @export
MRT_4_03_01 <- function(
  agg,
  agg_time = "year",
  ano,
  multi = 100000,
  decimals = 2,
  pop_source = "datasus",
  pcdas_token = NULL,
  adjust_rates = FALSE
) {
  # 1. Parâmetros e acesso aos dados
  agg_validas <- c(
    "mun_res", "mun_ocor",
    "regsaude_res", "regsaude_ocor",
    "regsaude_449_res", "regsaude_449_ocor",
    "uf_res", "uf_ocor"
  )

  if (!is.character(agg) || length(agg) != 1L ||
      is.na(agg) || !agg %in% agg_validas) {
    stop("'agg' deve ser uma agregação geográfica válida.", call. = FALSE)
  }
  if (!is.character(agg_time) || length(agg_time) != 1L ||
      is.na(agg_time) || !agg_time %in% c("year", "month", "week")) {
    stop("'agg_time' deve ser 'year', 'month' ou 'week'.", call. = FALSE)
  }
  if (!is.logical(adjust_rates) || length(adjust_rates) != 1L ||
      is.na(adjust_rates)) {
    stop("'adjust_rates' deve ser TRUE ou FALSE.", call. = FALSE)
  }
  if (is.null(pcdas_token)) {
    pcdas_token <- rpcdas::get_pcdas_token_renviron()
  }

  codigos_cid <- rpcdas::cid_seq("V01", "V89")
  filtro_causas <- paste0(
    "LEFT(CAUSABAS, 3) IN (",
    paste(sprintf("'%s'", codigos_cid), collapse = ", "),
    ")"
  )

  # 2. Taxa bruta
  if (!adjust_rates) {
    numerador <- rpcdas::get_sim(
      agg = agg,
      agg_time = agg_time,
      ano = ano,
      pcdas_token = pcdas_token,
      more_filters = filtro_causas
    )

    denominador <- brindi:::denominator_pop(
      agg = agg,
      pop_source = pop_source
    )

    return(brindi:::indicator_raw(
      numerador = numerador,
      denominador = denominador,
      denominador_type = "pop",
      treat_inf_values = TRUE,
      nome = "MRT_4_03_01",
      ano = ano,
      agg = agg,
      agg_time = agg_time,
      pop_source = pop_source,
      multi = multi,
      decimals = decimals
    ))
  }

  # 3. Taxa ajustada por idade
  plano_anterior <- future::plan(future::multisession)
  on.exit(future::plan(plano_anterior), add = TRUE)

  numerador <- furrr::future_pmap(
    .l = get("age_groups", envir = asNamespace("brindi")),
    .f = rpcdas::get_sim,
    agg = agg,
    agg_time = agg_time,
    ano = ano,
    pcdas_token = pcdas_token,
    more_filters = filtro_causas
  )

  brindi:::indicator_adjusted(
    numerador = numerador,
    ano = ano,
    agg = agg,
    agg_time = agg_time,
    pop_source = pop_source,
    nome = "MRT_4_03_01",
    multi = multi,
    decimals = decimals,
    sex = "all"
  )
}
