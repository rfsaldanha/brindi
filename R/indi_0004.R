#' Indicator: Mortalidade por Acidentes de transportes
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param ano numeric. Year of death.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#'
#' @examples
#' # Some examples
#' indi_0003(agg = "mun_res", ano = 2013)
#'
#' @importFrom rlang .data
#' @export
indi_0004 <- function(agg, ano, multi = 100000, decimals = 2, pcdas_token = NULL){

  # Try to get PCDaS API token from renviron if not provided
  if(is.null(pcdas_token)){
    pcdas_token <- rpcdas::get_pcdas_token_renviron()
  }

  # Creates numerator
  filter_query <- "LEFT(CAUSABAS, 3) IN ('V01', 'V02', 'V03', 'V04', 'V05', 'V06', 'V07', 'V08', 'V09', 'V10',
                                         'V11', 'V12', 'V13', 'V14', 'V15', 'V16', 'V17', 'V18', 'V19', 'V20',
                                         'V21', 'V22', 'V23', 'V24', 'V25', 'V26', 'V27', 'V28', 'V29', 'V30',
                                         'V31', 'V32', 'V33', 'V34', 'V35', 'V36', 'V37', 'V38', 'V39', 'V40',
                                         'V41', 'V42', 'V43', 'V44', 'V45', 'V46', 'V47', 'V48', 'V49', 'V50',
                                         'V51', 'V52', 'V53', 'V54', 'V55', 'V56', 'V57', 'V58', 'V59', 'V60',
                                         'V61', 'V62', 'V63', 'V64', 'V65', 'V66', 'V67', 'V68', 'V69', 'V70',
                                         'V71', 'V72', 'V73', 'V74', 'V75', 'V76', 'V77', 'V78', 'V79', 'V80',
                                         'V81', 'V82', 'V83', 'V84', 'V85', 'V86', 'V87', 'V88', 'V89')"
  numerador <- rpcdas::get_sim(
    agg = agg,
    ano = ano,
    pcdas_token = pcdas_token,
    more_filters = filter_query
  )

  # Creates denominator
  if(agg %in% c("mun_res", "mun_ocor")){
    denominador <- brpop::mun_pop_totals() %>%
      dplyr::rename(agg = .data$mun)
  } else if(agg %in% c("uf_res", "uf_ocor")){
    denominador <- brpop::uf_pop_totals() %>%
      dplyr::rename(agg = .data$uf)
  }

  # Join numerator and denominator, peform indicator calculus
  df <- dplyr::inner_join(x = numerador, y = denominador, by = c("agg" = "agg", "ano" = "year")) %>%
    dplyr::mutate(value = round(
      x = (.data$freq/.data$pop) * multi,
      digits = decimals
    )) %>%
    dplyr::select(-.data$freq, -.data$pop) %>%
    dplyr::mutate(name = "indi_0004") %>%
    dplyr::rename(cod = agg) %>%
    dplyr::mutate(agg = agg) %>%
    dplyr::relocate(.data$agg, .before = .data$cod) %>%
    dplyr::relocate(.data$value, .after = .data$name)

  return(df)
}

