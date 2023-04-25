#' Raw indicator calculus
#'
#' @param numerador character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param denominador numeric. Year of death.
#' @param denominador_type character. Type of denominator. \code{pop} or \code{arbitrary}.
#' @param nome character. PCDaS API token. If not provided, the function will look for it on renvirom.
#' @param agg character. Aggregation acronymin.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#'
#' @importFrom rlang .data
indicator_raw <- function(numerador, denominador, denominador_type = "pop", nome, agg, multi, decimals){

  if(denominador_type == "pop"){
    numerador <- numerador %>%
      dplyr::mutate(year = as.numeric(substring(.data$agg_time, 0, 4)))

    res <- dplyr::inner_join(x = numerador, y = denominador, by = c("agg" = "agg", "year" = "year")) %>%
      dplyr::mutate(value = round(
        x = (.data$freq/.data$pop) * multi,
        digits = decimals
      ))
  } else if(denominador_type == "arbitrary"){
    res <- dplyr::inner_join(x = numerador, y = denominador, by = c("agg" = "agg", "agg_time" = "agg_time")) %>%
      dplyr::mutate(value = round(
        x = (.data$freq.x/.data$freq.y) * multi,
        digits = decimals
      ))
  }

  res <- res %>%
    dplyr::mutate(name = nome) %>%
    dplyr::rename(cod = agg) %>%
    dplyr::mutate(agg = agg) %>%
    dplyr::relocate(.data$agg, .before = .data$cod) %>%
    dplyr::relocate(.data$value, .after = .data$name) %>%
    dplyr::select(.data$name, .data$cod, date = .data$agg_time, .data$value)

  return(res)
}
