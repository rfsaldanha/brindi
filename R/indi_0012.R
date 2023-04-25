#' Indicator: Mortalidade proporcional por infecção respiratória aguda em menores de 5 anos de idade
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param agg_time character. Time aggregation level. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param ano numeric. Year of death.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#' @param save_args logical. Save \code{agg} and \code{agg_time} arguments on results table.
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#'
#' @examples
#' # Some examples
#' indi_0012(agg = "mun_res", ano = 2013)
#'
#' @importFrom rlang .data
#' @export
indi_0012 <- function(agg, agg_time = "year", ano, multi = 100, decimals = 2, save_args = FALSE, pcdas_token = NULL){

  # Try to get PCDaS API token from renviron if not provided
  if(is.null(pcdas_token)){
    pcdas_token <- rpcdas::get_pcdas_token_renviron()
  }

  # Creates numerator
  numerador <- rpcdas::get_sim(
    agg = agg,
    agg_time = agg_time,
    ano = ano,
    idade_a = 0, idade_b = 4,
    cid_in = rpcdas::cid_seq("J00", "J22"),
    pcdas_token = pcdas_token
  )

  # Creates denominator
  denominador <- rpcdas::get_sim(
    agg = agg,
    agg_time = agg_time,
    ano = ano,
    idade_a = 0, idade_b = 4,
    pcdas_token = pcdas_token
  )


  # Perform indicator calculus
  res <- indicator_raw(
    numerador = numerador,
    denominador = denominador,
    denominador_type = "arbitrary",
    multi = multi,
    decimals = decimals,
    nome = "indi_0012",
    agg = agg
  )

  # Save arguments
  if(save_args == TRUE){
    res <- res %>%
      dplyr::mutate(
        agg = agg,
        agg_time = agg_time
      ) %>%
      dplyr::relocate(.data$agg, .data$agg_time, .after = .data$name)
  }

  return(res)
}

