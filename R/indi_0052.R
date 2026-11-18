#' Indicator: Proporção da população de 5 a 17 anos em situação de trabalho infantil
#'
#' @param agg character. Spatial aggregation level. \code{brasil} for Brazil
#' and \code{uf} for Unidade da Federação.
#' @param agg_time character. Time aggregation level. \code{year} for yearly
#' data. PNAD Continua-based indicators are annual. Defaults to \code{year}.
#' @param ano numeric. Reference year or vector of years.
#' @param multi integer. Multiplicator for indicator. Defaults to \code{100}.
#' @param decimals integer. Number of decimals for indicator. Defaults to \code{2}.
#' @param reload logical. Whether PNAD Continua files should be downloaded
#' again when already available in cache. Defaults to \code{FALSE}.
#' @param savedir character. Directory used to cache PNAD Continua files.
#' Defaults to the BRINDI user cache outside the package repository.
#'
#' @details
#' The indicator uses the PNAD Continua derived variable \code{SD06004},
#' which classifies people aged 5 to 17 years as being or not being in
#' child labour according to the IBGE methodology.
#'
#' The annual child-labour module is based on the fifth interview.
#' The currently supported years are 2016 to 2019 and 2022 to 2024.
#'
#' @examples
#' # Some examples
#' \dontrun{
#' indi_0052(agg = "brasil", ano = 2023)
#' indi_0052(agg = "uf", ano = 2023)
#' indi_0052(agg = "uf", ano = c(2022, 2023))
#' }
#'
#' @export
indi_0052 <- function(
  agg,
  agg_time = "year",
  ano,
  multi = 100,
  decimals = 2,
  reload = FALSE,
  savedir = file.path(tools::R_user_dir("brindi", "cache"), "pnadc")
) {

  # Check aggregation level
  if (!agg %in% c("brasil", "uf")) {
    stop(
      "`agg` must be either 'brasil' or 'uf'.",
      call. = FALSE
    )
  }

  # PNAD Continua annual indicators
  if (!identical(agg_time, "year")) {
    stop(
      "PNAD Continua-based indicators are annual. Use `agg_time = 'year'`.",
      call. = FALSE
    )
  }

  # Check years
  if (!is.numeric(ano) || length(ano) < 1L || any(is.na(ano))) {
    stop(
      "`ano` must be a valid year or vector of years.",
      call. = FALSE
    )
  }

  ano <- as.integer(ano)

  # Child-labour annual microdata currently available for these years
  valid_years <- c(2016L, 2017L, 2018L, 2019L, 2022L, 2023L, 2024L)

  if (any(!ano %in% valid_years)) {
    stop(
      paste0(
        "indi_0052 currently supports the PNAD Continua child-labour ",
        "module for years: ",
        paste(valid_years, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  # Create persistent cache outside the package repository
  if (!dir.exists(savedir)) {
    dir.create(
      savedir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  # Calculate one year at a time
  res <- lapply(ano, function(ano_i) {

    # Download/read annual PNAD Continua fifth-interview microdata
    #
    # design = FALSE is intentional. The survey design is created explicitly
    # with PNADcIBGE::pnadc_design() below, so users do not need to attach
    # PNADcIBGE with library(PNADcIBGE).
    pnad <- PNADcIBGE::get_pnadc(
      year = ano_i,
      interview = 5,
      selected = FALSE,
      vars = c(
        "UF",
        "V2009",
        "SD06004"
      ),
      labels = FALSE,
      deflator = FALSE,
      design = FALSE,
      reload = reload,
      savedir = savedir
    )

    if (is.null(pnad) || nrow(pnad) == 0L) {
      stop(
        paste0(
          "PNADcIBGE did not return child-labour microdata for year ",
          ano_i,
          "."
        ),
        call. = FALSE
      )
    }

    # Create PNAD Continua complex survey design
    pnad <- PNADcIBGE::pnadc_design(
      data_pnadc = pnad
    )

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

    # Defines denominator:
    # population aged 5 to 17 years with valid child-labour classification
    pnad <- subset(
      pnad,
      V2009 >= 5 &
        V2009 <= 17 &
        !is.na(SD06004)
    )

    if (nrow(pnad$variables) == 0L) {
      stop(
        paste0(
          "No valid observations were found for indi_0052 in year ",
          ano_i,
          "."
        ),
        call. = FALSE
      )
    }

    # Defines numerator:
    # population aged 5 to 17 years classified as being in child labour
    pnad <- stats::update(
      pnad,
      indicador = as.numeric(SD06004 == 1)
    )

    # Brazil
    if (agg == "brasil") {

      estimativa <- survey::svymean(
        ~indicador,
        design = pnad,
        na.rm = TRUE
      )

      return(
        tibble::tibble(
          nome = "indi_0052",
          ano = ano_i,
          agg = "brasil",
          valor = round(
            as.numeric(stats::coef(estimativa))[1] * multi,
            decimals
          )
        )
      )
    }

    # Unidade da Federação
    estimativa <- survey::svyby(
      ~indicador,
      ~UF,
      design = pnad,
      FUN = survey::svymean,
      na.rm = TRUE,
      keep.names = FALSE
    )

    tibble::tibble(
      nome = "indi_0052",
      ano = ano_i,
      agg = "uf",
      uf = estimativa$UF,
      valor = round(
        estimativa$indicador * multi,
        decimals
      )
    )
  })

  # Bind single or multiple years and keep BRINDI tibble output
  res <- do.call(rbind, res)
  res <- tibble::as_tibble(res)

  return(res)
}
