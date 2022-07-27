#' Population denominators
#'
#' This function retrieves populatin denominators.
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#'
#' @importFrom rlang .data
#' @export
denominator_pop <- function(agg){
  if(agg %in% c("mun_res", "mun_ocor")){
    denominador <- brpop::mun_pop_totals() %>%
      dplyr::rename(agg = .data$mun)
  } else if(agg %in% c("uf_res", "uf_ocor")){
    denominador <- brpop::uf_pop_totals() %>%
      dplyr::rename(agg = .data$uf)
  } else if(agg %in% c("regsaude_res", "regsaude_ocor")){
    denominador <- brpop::regsaude_pop_totals() %>%
      dplyr::rename(agg = .data$regsaude)
  }

  return(denominador)
}
