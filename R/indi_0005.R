#' Indicator: Mortalidade por suicidio
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saude of occurence. \code{regsaude_449_res} for regiao de saude (449 units) of residence. \code{regsaude_449_ocor} for regiao de saude (449 units) of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
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
#' indi_0005(agg = "mun_res", ano = 2013)
#'
#' @importFrom rlang .data
#' @export
indi_0005 <- function(
  agg,
  agg_time = "year",
  ano,
  multi = 100000,
  decimals = 2,
  pop_source = "datasus",
  pcdas_token = NULL,
  adjust_rates = FALSE
) {
  # Try to get PCDaS API token from renviron if not provided
  if (is.null(pcdas_token)) {
    pcdas_token <- rpcdas::get_pcdas_token_renviron()
  }

  Q1 <- glue::glue_collapse(
    sQuote(rpcdas::cid_seq("X60", "X84"), q = FALSE),
    sep = ", "
  )

  filter_query <- glue::glue("LEFT(CAUSABAS, 3) IN ({Q1}, 'X87')")

  if (adjust_rates == FALSE) {
    # Creates numerator
    numerador <- rpcdas::get_sim(
      agg = agg,
      agg_time = agg_time,
      ano = ano,
      pcdas_token = pcdas_token,
      more_filters = filter_query
    )

    # Creates denominator
    denominador <- denominator_pop(agg = agg, pop_source = pop_source)

    # Perform indicator calculus
    res <- indicator_raw(
      numerador = numerador,
      denominador = denominador,
      denominador_type = "pop",
      treat_inf_values = TRUE,
      nome = "indi_0005",
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
      .f = rpcdas::get_sim,
      agg = agg,
      agg_time = agg_time,
      ano = ano,
      more_filters = filter_query
    )

    # Age adjusted indicator computation
    res <- indicator_adjusted(
      numerador = numerador,
      ano = ano,
      agg = agg,
      agg_time = agg_time,
      pop_source = pop_source,
      nome = "indi_0005",
      multi = multi,
      decimals = decimals
    )
  }

  return(res)
}
