#' Raw indicator calculus
#'
#' @param numerador character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param denominador numeric. Year of death.
#' @param denominador_type character. Type of denominator. \code{pop} or \code{arbitrary}.
#' @param treat_inf_values logical. Treat positive infinite values caused by denominators equals to zero as NA. Defaults to \code{TRUE}.
#' @param nome character. Indicator name.
#' @param ano Year of indicator
#' @param agg character. Aggregation acronymin.
#' @param agg_time character. Time aggregation level. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param pop_source character. Population source, from {brpop} package.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#'
#' @importFrom rlang .data
indicator_raw <- function(
  numerador,
  denominador,
  denominador_type = "pop",
  treat_inf_values = TRUE,
  nome,
  ano,
  agg,
  agg_time,
  pop_source,
  multi,
  decimals
) {
  # Calculate indicator considering a population denominator
  if (denominador_type == "pop") {
    numerador <- numerador %>%
      dplyr::mutate(year = as.numeric(substring(.data$agg_time, 0, 4)))

    res <- dplyr::inner_join(
      x = numerador,
      y = denominador,
      by = c("agg" = "agg", "year" = "year")
    ) %>%
      dplyr::mutate(
        value = round(
          x = (.data$freq / .data$pop) * multi,
          digits = decimals
        )
      )
    # Calculate indicator considering an arbitrary denominator
  } else if (denominador_type == "arbitrary") {
    res <- dplyr::inner_join(
      x = numerador,
      y = denominador,
      by = c("agg" = "agg", "agg_time" = "agg_time")
    ) %>%
      dplyr::rename(freq = .data$freq.x, pop = .data$freq.y) %>%
      dplyr::mutate(
        value = round(
          x = (.data$freq / .data$pop) * multi,
          digits = decimals
        )
      )
  }

  # Treat positive Inf values
  if (treat_inf_values == TRUE) {
    res <- res %>%
      dplyr::mutate(
        value = dplyr::case_when(
          .data$pop == 0 ~ NA_real_,
          .default = .data$value
        )
      )
  }

  # Organize results
  res <- res %>%
    dplyr::mutate(name = nome) %>%
    dplyr::rename(cod = agg) %>%
    dplyr::mutate(agg = agg) %>%
    dplyr::relocate(.data$agg, .before = .data$cod) %>%
    dplyr::relocate(.data$value, .after = .data$name)

  # Select fields
  res <- res %>%
    dplyr::select(.data$name, .data$cod, date = .data$agg_time, .data$value)

  # Complete with zeros
  res <- complete_with_zeros(
    res = res,
    agg = agg,
    agg_time = agg_time,
    ano = ano,
    pop_source = pop_source
  )

  return(res)
}
