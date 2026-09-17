#' Indicator: Razão entre a renda total dos 10% mais ricos e a dos 40% mais pobres
#'
#' @param agg character. Spatial aggregation level. \code{brasil} for Brazil,
#' \code{uf} for Unidade da Federação, and \code{capital} for state capitals
#' and Distrito Federal.
#' @param agg_time character. Time aggregation level. \code{year} for yearly
#' data. PNAD Continua-based indicators are annual. Defaults to \code{year}.
#' @param ano numeric. Reference year or vector of years.
#' @param decimals integer. Number of decimals for indicator. Defaults to \code{2}.
#' @param reload logical. Whether PNAD Continua files should be downloaded
#' again when already available in cache. Defaults to \code{FALSE}.
#' @param savedir character. Directory used to cache PNAD Continua files.
#' Defaults to the BRINDI user cache outside the package repository.
#'
#' @details
#' This indicator corresponds to the Palma ratio (RIPSA SOC.3.04):
#' the total household per capita income of the richest 10 percent divided by
#' the total household per capita income of the poorest 40 percent.
#'
#' The distribution is defined using \code{VD5008}, the household per capita
#' income based on habitual income from all jobs and effective income from
#' other sources, excluding income of pensioners, resident domestic workers,
#' and relatives of resident domestic workers.
#'
#' People whose household condition is pensioner, resident domestic worker,
#' or relative of a resident domestic worker (\code{V2005} equal to 17, 18,
#' or 19) are excluded from the analytical population, following the RIPSA
#' definition.
#'
#' Annual microdata from the first interview are used, except for 2020, 2021,
#' and 2022, for which the fifth interview is used, following RIPSA/IBGE
#' methodology.
#'
#' The indicator is calculated using weighted income quantiles and totals from
#' the complex PNAD Continua survey design. Income values are not deflated
#' because a common deflator does not change the ratio.
#'
#' @examples
#' # Some examples
#' \dontrun{
#' indi_0053(agg = "brasil", ano = 2023)
#' indi_0053(agg = "uf", ano = 2023)
#' indi_0053(agg = "capital", ano = 2023)
#' indi_0053(agg = "capital", ano = c(2022, 2023))
#' }
#'
#' @export
indi_0053 <- function(
  agg,
  agg_time = "year",
  ano,
  decimals = 2,
  reload = FALSE,
  savedir = file.path(tools::R_user_dir("brindi", "cache"), "pnadc")
) {

  # Check aggregation level
  if (!agg %in% c("brasil", "uf", "capital")) {
    stop(
      "`agg` must be one of: 'brasil', 'uf', or 'capital'.",
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

  if (any(ano < 2012L)) {
    stop(
      "indi_0053 supports PNAD Continua annual microdata from 2012 onward.",
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

  # Internal calculator for one survey domain
  calc_palma <- function(design) {

    # convey requires preparation of the survey design before
    # concentration/inequality estimators are calculated
    design <- convey::convey_prep(design)

    # Total income of the poorest 40%
    bottom_40 <- convey::svyisq(
      ~VD5008,
      design = design,
      alpha = 0.40,
      upper = FALSE,
      na.rm = TRUE
    )

    # Total income of the richest 10% (above the 90th percentile)
    top_10 <- convey::svyisq(
      ~VD5008,
      design = design,
      alpha = 0.90,
      upper = TRUE,
      na.rm = TRUE
    )

    bottom_value <- as.numeric(stats::coef(bottom_40))[1]
    top_value <- as.numeric(stats::coef(top_10))[1]

    if (
      !is.finite(bottom_value) ||
      !is.finite(top_value) ||
      bottom_value <= 0
    ) {
      return(NA_real_)
    }

    top_value / bottom_value
  }

  # Calculate one year at a time
  res <- lapply(ano, function(ano_i) {

    # RIPSA uses the fifth interview for 2020-2022 because of
    # pandemic-related collection losses; otherwise it uses first interview
    interview_i <- if (ano_i %in% 2020:2022) 5L else 1L

    # Download/read annual PNAD Continua microdata
    #
    # design = FALSE is intentional. The survey design is created explicitly
    # with PNADcIBGE::pnadc_design() below, so users do not need to attach
    # PNADcIBGE with library(PNADcIBGE).
    pnad <- PNADcIBGE::get_pnadc(
      year = ano_i,
      interview = interview_i,
      selected = FALSE,
      vars = c(
        "UF",
        "Capital",
        "V2005",
        "VD5008"
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
          "PNADcIBGE did not return income microdata for year ",
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

    # Analytical population:
    # excludes pensioners, resident domestic workers and their relatives,
    # and observations without valid household per capita income
    pnad <- subset(
      pnad,
      !(V2005 %in% c(17, 18, 19)) &
        !is.na(VD5008) &
        VD5008 >= 0
    )

    if (nrow(pnad$variables) == 0L) {
      stop(
        paste0(
          "No valid observations were found for indi_0053 in year ",
          ano_i,
          "."
        ),
        call. = FALSE
      )
    }

    # Brazil
    if (agg == "brasil") {
      return(
        tibble::tibble(
          nome = "indi_0053",
          ano = ano_i,
          agg = "brasil",
          valor = round(
            calc_palma(pnad),
            decimals
          )
        )
      )
    }

    # Unidade da Federação
    if (agg == "uf") {

      uf_values <- sort(
        unique(pnad$variables$UF[!is.na(pnad$variables$UF)])
      )

      out <- lapply(uf_values, function(uf_i) {

        design_i <- subset(
          pnad,
          UF == uf_i
        )

        tibble::tibble(
          nome = "indi_0053",
          ano = ano_i,
          agg = "uf",
          uf = uf_i,
          valor = round(
            calc_palma(design_i),
            decimals
          )
        )
      })

      return(
        tibble::as_tibble(
          do.call(rbind, out)
        )
      )
    }

    # Capitals and Distrito Federal
    pnad_capital <- subset(
      pnad,
      !is.na(Capital)
    )

    capital_values <- sort(
      unique(
        pnad_capital$variables$Capital[
          !is.na(pnad_capital$variables$Capital)
        ]
      )
    )

    out <- lapply(capital_values, function(capital_i) {

      design_i <- subset(
        pnad_capital,
        Capital == capital_i
      )

      tibble::tibble(
        nome = "indi_0053",
        ano = ano_i,
        agg = "capital",
        capital = capital_i,
        valor = round(
          calc_palma(design_i),
          decimals
        )
      )
    })

    tibble::as_tibble(
      do.call(rbind, out)
    )
  })

  # Bind single or multiple years and keep BRINDI tibble output
  res <- do.call(rbind, res)
  res <- tibble::as_tibble(res)

  return(res)
}
