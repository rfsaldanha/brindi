#' Expand indicator results with arguments with parallel processing
#'
#' @param agg character vector. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param anos numeric vector. Years
#' @param indi_fun Function, like `indi_0001` or `indi_0002`.
#'
#' @examples
#' # Some examples
#' expand_indi(agg = c("uf_res", "uf_ocor"), anos = c(2013, 2014, 2015), indi_fun = indi_0001)
#'
#' @export
expand_indi <- function(agg, anos, indi_fun){
  oplan <- future::plan(future::multisession)
  on.exit(future::plan(oplan))

  job <- tidyr::crossing(
    agg = agg,
    anos = anos
  )

  res <- furrr::future_map2_dfr(
    .x = job$agg,
    .y = job$anos,
    .f = indi_fun
  )

  return(res)
}
