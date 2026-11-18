#' Indicator: Proporção da população ocupada sem contribuição para a previdência social
#'
#' @param agg character. Spatial aggregation level. \code{brasil} for Brazil
#' and \code{uf} for Unidade da Federação.
#' @param agg_time character. Time aggregation level. \code{year} for yearly
#' data. PNAD Continua-based indicators are annual. Defaults to \code{year}.
#' @param ano numeric. Reference year or vector of years.
#' @param multi integer. Multiplicator for indicator. Defaults to \code{100}.
#' @param decimals integer. Number of decimals for indicator. Defaults to \code{2}.
#' @param interview integer. PNAD Continua interview used to retrieve annual
#' microdata. Defaults to \code{1}.
#' @param reload logical. Whether PNAD Continua files should be downloaded
#' again when already available in cache. Defaults to \code{FALSE}.
#' @param savedir character. Directory used to cache PNAD Continua files.
#' Defaults to the BRINDI user cache outside the package repository.
#'
#' @examples
#' # Some examples
#' \dontrun{
#' indi_0050(agg = "brasil", ano = 2023)
#' indi_0050(agg = "uf", ano = 2023)
#' indi_0050(agg = "uf", ano = c(2022, 2023))
#' }
#'
#' @export
indi_0050 <- function(
  agg,
  agg_time = "year",
  ano,
  multi = 100,
  decimals = 2,
  interview = 1,
  reload = FALSE,
  savedir = file.path(tools::R_user_dir("brindi", "cache"), "pnadc")
) {

  if (!agg %in% c("brasil", "uf")) {
    stop("`agg` must be either 'brasil' or 'uf'.", call. = FALSE)
  }

  if (!identical(agg_time, "year")) {
    stop(
      "PNAD Continua-based indicators are annual. Use `agg_time = 'year'`.",
      call. = FALSE
    )
  }

  if (!is.numeric(ano) || length(ano) < 1L || any(is.na(ano))) {
    stop("`ano` must be a valid year or vector of years.", call. = FALSE)
  }

  ano <- as.integer(ano)

  if (!dir.exists(savedir)) {
    dir.create(savedir, recursive = TRUE, showWarnings = FALSE)
  }

  res <- lapply(ano, function(ano_i) {

    pnad <- PNADcIBGE::get_pnadc(
      year = ano_i,
      interview = interview,
      selected = FALSE,
      vars = c("UF", "V2009", "VD4002", "VD4012"),
      labels = FALSE,
      deflator = FALSE,
      design = FALSE,
      reload = reload,
      savedir = savedir
    )

    if (is.null(pnad) || nrow(pnad) == 0L) {
      stop(
        paste0("PNADcIBGE did not return data for year ", ano_i, "."),
        call. = FALSE
      )
    }

    pnad <- PNADcIBGE::pnadc_design(data_pnadc = pnad)

    if (!inherits(pnad, c("survey.design", "survey.design2", "svyrep.design"))) {
      stop(
        paste0(
          "PNADcIBGE could not create the survey design for year ",
          ano_i,
          "."
        ),
        call. = FALSE
      )
    }

    pnad <- subset(
      pnad,
      V2009 >= 14 &
        VD4002 == 1 &
        !is.na(VD4012)
    )

    if (nrow(pnad$variables) == 0L) {
      stop(
        paste0(
          "No valid observations were found for indi_0050 in year ",
          ano_i,
          "."
        ),
        call. = FALSE
      )
    }

    pnad <- stats::update(
      pnad,
      indicador = as.numeric(VD4012 == 2)
    )

    if (agg == "brasil") {
      estimativa <- survey::svymean(
        ~indicador,
        design = pnad,
        na.rm = TRUE
      )

      return(
        tibble::tibble(
          nome = "indi_0050",
          ano = ano_i,
          agg = "brasil",
          valor = round(
            as.numeric(stats::coef(estimativa))[1] * multi,
            decimals
          )
        )
      )
    }

    estimativa <- survey::svyby(
      ~indicador,
      ~UF,
      design = pnad,
      FUN = survey::svymean,
      na.rm = TRUE,
      keep.names = FALSE
    )

    tibble::tibble(
      nome = "indi_0050",
      ano = ano_i,
      agg = "uf",
      uf = estimativa$UF,
      valor = round(
        estimativa$indicador * multi,
        decimals
      )
    )
  })

  res <- do.call(rbind, res)
  res <- tibble::as_tibble(res)

  return(res)
}
