#' Indicator: Mortalidade proporcional por doença diarreica aguda em menores de 5 anos
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param ano numeric. Year of death.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#'
#' @examples
#' # Some examples
#' indi_0009(agg = "mun_res", ano = 2013)
#'
#' @importFrom rlang .data
#' @export
indi_0009 <- function(agg, ano, multi = 100000, decimals = 2, pcdas_token = NULL){

  # Try to get PCDaS API token from renviron if not provided
  if(is.null(pcdas_token)){
    pcdas_token <- rpcdas::get_pcdas_token_renviron()
  }

  # Creates numerator
  Q1 <- glue::glue_collapse(sQuote(rpcdas::cid_seq("A00","A09"), q = FALSE), sep = ", ")

  numerador <- rpcdas::get_sim(
    agg = agg,
    ano = ano,
    idade_a = 0,
    idade_b = 4,
    pcdas_token = pcdas_token,
    more_filters = Q1
  )

  # Creates denominator
  denominador <- denominator_pop(agg = agg, age_group_vec = "0 to 4 years")

  # Join numerator and denominator, peform indicator calculus
  df <- dplyr::inner_join(x = numerador, y = denominador, by = c("agg" = "agg", "ano" = "year")) %>%
    dplyr::mutate(value = round(
      x = (.data$freq/.data$pop) * multi,
      digits = decimals
    )) %>%
    dplyr::select(-.data$freq, -.data$pop) %>%
    dplyr::mutate(name = "indi_0009") %>%
    dplyr::rename(cod = agg) %>%
    dplyr::mutate(agg = agg) %>%
    dplyr::relocate(.data$agg, .before = .data$cod) %>%
    dplyr::relocate(.data$value, .after = .data$name)

  return(df)
}

