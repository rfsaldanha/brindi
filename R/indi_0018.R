#' Indicator: Taxa de incidência de febre amarela
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{regsaude_449_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saude of occurence. \code{regsaude_449_res} for regiao de saude (449 units) of residence. \code{regsaude_449_ocor} for regiao de saude (449 units) of occurence. \code{mun_res} for municipality of residence.
#' @param agg_time character. Time aggregation level. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param ano numeric. Year of death.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#' @param complete_with_zeros logical. Complete indicator result with zeros considering combinations of spatial and temporal aggregation without results.
#' @param keep_raw_values logical. Keep numerator and denominator values on results.
#' @param save_args logical. Save \code{agg} and \code{agg_time} arguments on results table.
#'
#' @importFrom rlang .data
#' @export
indi_0018 <- function(agg, agg_time = "year", ano, multi = 100000, decimals = 2, complete_with_zeros = TRUE, keep_raw_values = FALSE, save_args = FALSE){

  # Creates numerator
  numerador <- recbilis::get_febre_amarela(
    agg = agg,
    agg_time = agg_time,
    ano = ano
  )

  # Creates denominator
  denominador <- denominator_pop(agg = agg)

  # Perform indicator calculus
  res <- indicator_raw(
    numerador = numerador,
    denominador = denominador,
    multi = multi,
    decimals = decimals,
    keep_raw_values = keep_raw_values,
    nome = "indi_0018",
    agg = agg
  )

  # Save arguments
  if(save_args == TRUE){
    res <- res %>%
      dplyr::mutate(
        agg = agg,
        agg_time = agg_time
      ) %>%
      dplyr::relocate("agg", "agg_time", .after = "name")
  }

  # Complete with zeros
  if(complete_with_zeros == TRUE){
    res <- complete_with_zeros(res = res, agg = agg, agg_time = agg_time,
                               ano = ano, save_args = save_args)
  }

  return(res)
}

