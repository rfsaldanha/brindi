#' Expand several indicators results to SQLite
#'
#' @param agg character vector. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param agg_time character vector. Time aggregation levels. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param anos numeric vector. Years
#' @param dir character. Directory to save parquet files.
#' @param indi character function names vector. Defaults to `all` for all indi_ functions.
#' @param pop_source character. Population source, from {brpop} package.
#' @param adjust_rates logical. Adjust rates by age.
#'
#' @export
expand_indi_parquet <- function(
  agg,
  agg_time,
  anos,
  dir,
  indi,
  pop_source = "datasus",
  adjust_rates = FALSE
) {
  # List indi_ functions or use supplied vector
  if (length(indi) == 1) {
    if (indi == "all") {
      indi_funs <- grep("^indi_", ls(getNamespace("brindi")), value = TRUE)
    } else {
      indi_funs <- c(indi)
    }
  } else {
    indi_funs <- c(indi)
  }

  # Expand indi_functions and write parquet files
  for (i in indi_funs) {
    tmp <- expand_indi(
      agg = agg,
      agg_time = agg_time,
      anos = anos,
      indi_fun = i,
      pop_source = pop_source,
      adjust_rates = adjust_rates
    )
    arrow::write_parquet(
      x = tmp,
      sink = glue::glue("{dir}/{as.character(i)}_{agg}_{agg_time}.parquet")
    )
  }
}
