#' Indicator: Taxa de mortalidade por agressão
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for health region of residence. \code{regsaude_ocor} for health region of occurrence. \code{regsaude_449_res} for health region (449 units) of residence. \code{regsaude_449_ocor} for health region (449 units) of occurrence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of occurrence.
#' @param agg_time character. Time aggregation level. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param ano numeric. Year of death.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#' @param pop_source character. Population source, from {brpop} package.
#' @param adjust_rates logical. Adjust rates by age.
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renviron.
#'
#' @examples
#' # Some examples
#' \dontrun{
#' indi_0049(agg = "mun_res", ano = 2013)
#' }
#'
#' @importFrom rlang .data
#' @export
indi_0049 <- function(
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
  
  if (adjust_rates == FALSE) {
    # Creates numerator
    numerador <- rpcdas::get_sim(
      agg = agg,
      agg_time = agg_time,
      ano = ano,
      pcdas_token = pcdas_token,
      cid_in = c(
        # Firearm assault
        rpcdas::cid_seq("X93", "X95"),
        
        # Sharp object, cutting/piercing/blunt object assault
        "X99",
        "Y00",
        
        # Assault by strangulation
        "X91",
        
        # Assault by bodily force
        "Y04",
        
        # Other specified means
        rpcdas::cid_seq("X85", "X90"),
        "X92",
        rpcdas::cid_seq("X96", "X98"),
        rpcdas::cid_seq("Y01", "Y03"),
        rpcdas::cid_seq("Y05", "Y08"),
        
        # Unspecified means
        "Y09"
      )
    )
    
    # Creates denominator
    denominador <- denominator_pop(agg = agg, pop_source = pop_source)
    
    # Performs indicator calculation
    res <- indicator_raw(
      numerador = numerador,
      denominador = denominador,
      denominador_type = "pop",
      treat_inf_values = TRUE,
      nome = "indi_0049",
      ano = ano,
      agg = agg,
      agg_time = agg_time,
      pop_source = pop_source,
      multi = multi,
      decimals = decimals
    )
  } else if (adjust_rates == TRUE) {
    # Prepare multisession environment
    oplan <- future::plan(future::multisession)
    on.exit(future::plan(oplan))
    
    # Creates numerator
    numerador <- furrr::future_pmap(
      .l = age_groups,
      .f = rpcdas::get_sim,
      agg = agg,
      agg_time = agg_time,
      ano = ano,
      pcdas_token = pcdas_token,
      cid_in = c(
        # Firearm assault
        rpcdas::cid_seq("X93", "X95"),
        
        # Sharp object, cutting/piercing/blunt object assault
        "X99",
        "Y00",
        
        # Assault by strangulation
        "X91",
        
        # Assault by bodily force
        "Y04",
        
        # Other specified means
        rpcdas::cid_seq("X85", "X90"),
        "X92",
        rpcdas::cid_seq("X96", "X98"),
        rpcdas::cid_seq("Y01", "Y03"),
        rpcdas::cid_seq("Y05", "Y08"),
        
        # Unspecified means
        "Y09"
      )
    )
    
    # Age-adjusted indicator computation
    res <- indicator_adjusted(
      numerador = numerador,
      ano = ano,
      agg = agg,
      agg_time = agg_time,
      pop_source = pop_source,
      nome = "indi_0049",
      multi = multi,
      decimals = decimals,
      sex = "all"
    )
  }
  
  return(res)
}