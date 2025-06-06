#' Expand one indicator results with combination of arguments
#'
#' With parallel processing.
#'
#' @param agg character vector. Spatial aggregation levell. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param agg_time character vector. Time aggregation levels. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param anos numeric vector. Years.
#' @param indi_fun character. Function name.
#'
#' @export
expand_indi <- function(
  agg,
  agg_time,
  anos,
  indi_fun,
  pop_source,
  adjust_rates
) {
  # Start parallel enviroment
  plan <- future::plan(future::multisession)
  on.exit(future::plan(plan))

  # Plan tasks
  job <- tidyr::crossing(
    agg = agg,
    agg_time = agg_time
  )

  # Execute
  res <- furrr::future_map2_dfr(
    .x = job$agg,
    .y = job$agg_time,
    .f = get(indi_fun),
    ano = anos,
    pop_source = pop_source,
    adjust_rates = adjust_rates,
    .options = furrr::furrr_options(seed = TRUE)
  )

  return(res)
}
