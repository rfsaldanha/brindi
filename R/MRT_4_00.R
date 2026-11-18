#' MRT.4.00 | Taxa de mortalidade por causas externas
#'
#' @description
#' Categoria: Mortalidade | Dimensão 4: Mortalidade por causas externas.
#'
#' Estima a mortalidade por causas externas (CID-10: V01–Y98)
#' por 100.000 habitantes, com registros do SIM e dados populacionais.
#' Permite agregação por município, região de saúde ou UF, segundo
#' residência ou ocorrência, com taxas brutas ou ajustadas por idade.
#'
#' @details
#' Fonte: SIM/DATASUS (PCDaS) | População: brpop.
#' Cálculo: (óbitos / população) * 100.000.
#' Nota: na agregação por ocorrência, o denominador é a população
#' residente do território, o que exige cautela na interpretação.
#'
#' @param agg Agregação geográfica: `mun_res` (município de residência),
#'   `mun_ocor` (município de ocorrência), `regsaude_res` (região de saúde de
#'   residência), `regsaude_ocor` (região de saúde de ocorrência), `uf_res`
#'   (UF de residência) ou `uf_ocor` (UF de ocorrência).
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
#' MRT_4_00(agg = "mun_res", ano = 2020)
#' MRT_4_00(agg = "regsaude_ocor", ano = 2020)
#' MRT_4_00(agg = "uf_res", ano = 2020, adjust_rates = TRUE)
#' }
#'
#' @importFrom rlang .data
#' @export
MRT_4_00 <- function(
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
    "uf_res", "uf_ocor"
  )
  if (length(agg) != 1L || is.na(agg) || !agg %in% agg_validas) {
    stop("'agg' deve ser uma agregação geográfica válida.", call. = FALSE)
  }
  if (length(agg_time) != 1L || is.na(agg_time) ||
      !agg_time %in% c("year", "month", "week")) {
    stop("'agg_time' deve ser 'year', 'month' ou 'week'.", call. = FALSE)
  }
  if (length(adjust_rates) != 1L || is.na(adjust_rates) ||
      !is.logical(adjust_rates)) {
    stop("'adjust_rates' deve ser TRUE ou FALSE.", call. = FALSE)
  }
  if (is.null(pcdas_token)) {
    pcdas_token <- rpcdas::get_pcdas_token_renviron()
  }

  filtro_causas <- "LEFT(CAUSABAS, 1) IN ('V', 'W', 'X', 'Y')"

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
      nome = "MRT_4_00",
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
    nome = "MRT_4_00",
    multi = multi,
    decimals = decimals,
    sex = "all"
  )
}
