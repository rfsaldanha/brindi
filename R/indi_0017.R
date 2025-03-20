#' Indicator: Taxa de incidência de chikungunya
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{regsaude_449_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saude of occurence. \code{regsaude_449_res} for regiao de saude (449 units) of residence. \code{regsaude_449_ocor} for regiao de saude (449 units) of occurence. \code{mun_res} for municipality of residence.
#' @param agg_time character. Time aggregation level. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param ano numeric. Year of death.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#' @param pop_source character. Population source, from {brpop} package.
#' @param adjust_rates logical. Adjust rates by age.
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#'
#' @examples
#' # Some examples
#' indi_0017(agg = "mun_res", ano = 2020)
#'
#' @importFrom rlang .data
#' @export
indi_0017 <- function(
  agg,
  agg_time = "year",
  ano,
  multi = 100000,
  decimals = 2,
  pop_source = "datasus",
  pcdas_token = NULL,
  adjust_rates = FALSE
) {
  if (adjust_rates == FALSE) {
    # Creates numerator
    numerador <- recbilis::get_chikungunya(
      agg = agg,
      agg_time = agg_time,
      ano = ano
    )

    # Creates denominator
    denominador <- denominator_pop(agg = agg, pop_source = pop_source)

    # Perform indicator calculus
    res <- indicator_raw(
      numerador = numerador,
      denominador = denominador,
      denominador_type = "pop",
      treat_inf_values = TRUE,
      nome = "indi_0017",
      ano = ano,
      agg = agg,
      agg_time = agg_time,
      pop_source = pop_source,
      multi = multi,
      decimals = decimals
    )
  } else if (adjust_rates == TRUE) {
    # Prepate multission environment
    oplan <- future::plan(future::multisession)
    on.exit(future::plan(oplan))

    # Creates numerator
    numerador <- furrr::future_pmap(
      .l = age_groups,
      .f = recbilis::get_chikungunya,
      agg = agg,
      agg_time = agg_time,
      ano = ano
    )

    # Age adjusted indicator computation
    res <- indicator_adjusted(
      numerador = numerador,
      ano = ano,
      agg = agg,
      agg_time = agg_time,
      pop_source = pop_source,
      nome = "indi_0017",
      multi = multi,
      decimals = decimals,
      sex = "all"
    )
  }

  return(res)
}
