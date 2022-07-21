#' Indicator: Mortalidade por causas externas
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param ano numeric. Year of death.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#'
#' @examples
#' # Some examples
#' indi_0001(agg = "mun_res", ano = 2013)
#'
#' @importFrom rlang .data
#' @importFrom foreach %dopar%
#' @export
indi_0001 <- function(agg, ano, multi = 100000, decimals = 2, pcdas_token = NULL){

  # Try to get PCDaS API token from renviron if not provided
  if(is.null(pcdas_token)){
    pcdas_token <- get_pcdas_token_renviron()
  }

  # Creates numerator
  filter_query <- "LEFT(CAUSABAS, 1) IN ('V', 'W', 'X', 'Y')"
  if(length(ano) == 1){
    numerador <- get_sim(
      agg = agg, ano = ano,
      pcdas_token = pcdas_token,
      more_filters = filter_query
    )
  } else if(length(ano) > 1){
    # Start parallel cluster
    doParallel::registerDoParallel(parallel::detectCores() - 1)

    # Get numerator in parallel
    a <- NULL
    numerador <- foreach::foreach(a = ano, .combine = dplyr::bind_rows) %dopar% {
      get_sim(
        agg = agg, ano = a,
        pcdas_token = pcdas_token,
        more_filters = filter_query
      )
    }

    # Stop cluster
    doParallel::stopImplicitCluster()
  }

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
    dplyr::mutate(name = "indi_0001") %>%
    dplyr::relocate(.data$name, .after = .data$agg)

  return(df)
}
