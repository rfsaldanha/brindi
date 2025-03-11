#' Indicator: Mortalidade por causas externas
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param ano numeric. Year of death.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#'
#' @examples
#' # Some examples
#' indi_0001(agg = "mun_res", ano = 2013)
#'
#' @importFrom rlang .data
#' @export
indi_0001_adj <- function(
  agg,
  ano,
  multi = 100000,
  decimals = 2,
  pcdas_token = NULL,
  adjust_rates = FALSE
) {
  # Try to get PCDaS API token from renviron if not provided
  if (is.null(pcdas_token)) {
    pcdas_token <- rpcdas::get_pcdas_token_renviron()
  }

  filter_query <- "LEFT(CAUSABAS, 1) IN ('V', 'W', 'X', 'Y')"

  if (adjust_rates == FALSE) {
    # Creates numerator
    numerador <- rpcdas::get_sim(
      agg = agg,
      ano = ano,
      pcdas_token = pcdas_token,
      more_filters = filter_query
    )

    # Creates denominator
    denominador <- denominator_pop(agg = agg)

    # Perform indicator calculus
    res <- indicator_raw(
      numerador = numerador,
      denominador = denominador,
      multi = multi,
      decimals = decimals,
      nome = "indi_0001",
      agg = agg
    )
  } else if (adjust_rates == TRUE) {
    # Prepate multission environment
    oplan <- future::plan(future::multisession)
    on.exit(future::plan(oplan))

    # Creates numerator
    numerador <- furrr::future_pmap(
      .l = age_groups,
      .f = rpcdas::get_sim,
      agg = agg,
      ano = ano,
      more_filters = filter_query
    )

    # Age adjusted indicator calculus
    res <- indicator_adjusted()
  }

  return(res)
}
