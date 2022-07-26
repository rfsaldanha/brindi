#' Indicator: Mortalidade por AVC
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param ano numeric. Year of death.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#'
#' @examples
#' # Some examples
#' indi_0002(agg = "mun_res", ano = 2013)
#'
#' @importFrom rlang .data
#' @export
indi_0002 <- function(agg, ano, multi = 100000, decimals = 2, pcdas_token = NULL){

  # Try to get PCDaS API token from renviron if not provided
  if(is.null(pcdas_token)){
    pcdas_token <- rpcdas::get_pcdas_token_renviron()
  }

  # Multisession plan
  oplan <- future::plan(future::multisession)
  on.exit(future::plan(oplan))

  # Creates numerator
  numerador <- furrr::future_map_dfr(
    .x = ano,
    .f = rpcdas::get_sim,
    agg = agg,
    pcdas_token = pcdas_token,
    cid_like = "I6"
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
    dplyr::mutate(indi = round(
      x = (.data$freq/.data$pop) * multi,
      digits = decimals
    )) %>%
    dplyr::select(-.data$freq, -.data$pop) %>%
    dplyr::mutate(name = "indi_0002") %>%
    dplyr::relocate(.data$name, .after = .data$agg)

  return(df)
}
