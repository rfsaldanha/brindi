#' Expand indicator results with arguments with parallel processing
#'
#' @param agg character vector. Spatial aggregation levell. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param agg_time character vector. Time aggregation levels. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param anos numeric vector. Years.
#' @param indi_fun character. Function name.
#' @param save_args logical. Save \code{agg} and \code{agg_time} arguments on results table.
#'
#' @examples
#' # Some examples
#' expand_indi(agg = c("uf_res", "uf_ocor"), anos = c(2013, 2014, 2015), indi_fun = "indi_0001")
#'
#' @export
expand_indi <- function(agg, agg_time, anos, indi_fun, save_args = TRUE){
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
    save_args = TRUE
  )

  return(res)
}
