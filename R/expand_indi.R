#' Expand one indicator results with combination of arguments
#'
#' With parallel processing.
#'
#' @param agg character vector. Spatial aggregation levell. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param agg_time character vector. Time aggregation levels. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param anos numeric vector. Years.
#' @param indi_fun character. Function name.
#' @param save_args logical. Save \code{agg} and \code{agg_time} arguments on results table.
#'
#' @export
expand_indi <- function(agg, agg_time, anos, indi_fun, save_args = TRUE){
  # Start parallel enviroment
  plan <- future::plan(future::multisession)
  on.exit(future::plan(plan))

  # Plan tasks
  job <- tidyr::crossing(
    agg = agg,
    agg_time = agg_time,
    anos = anos,
    indi = indi_fun
  )

  # Execute
  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(job))
    res <- furrr::future_pmap(
      .l = list(
        indi = job$indi,
        agg = job$agg,
        agg_time = job$agg_time,
        ano = job$anos
      ),
      save_args = save_args,
      engine = "psql",
      psql_args = list(psql_db = "harmo", psql_host = "localhost", psql_port = 5432, psql_schema = "public", psql_table = "teste", psql_user = NULL, psql_pwd = NULL),
      p = p,
      .f = do_call_indi,
      .options = furrr::furrr_options(seed = TRUE)
    )
  })



  return(res)
}
