#' Indicator: Taxa de incidência de dengue clássico
#'
#' @param agg character. \code{mun_res} for municipality of residence.
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
indi_0015 <- function(agg, agg_time = "year", ano, multi = 100000, decimals = 2, complete_with_zeros = TRUE, keep_raw_values = FALSE, save_args = FALSE){

  # Creates numerator
  numerador <- recbilis::get_dengue(
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
    nome = "indi_0015",
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

  # Complete with zeros
  if(complete_with_zeros == TRUE){
    res <- complete_with_zeros(res = res, agg = agg, agg_time = agg_time,
                               ano = ano, save_args = save_args)
  }

  return(res)
}

