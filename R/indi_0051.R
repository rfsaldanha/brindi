#' Indicator: Proporção de jovens que não estudam e não trabalham
#'
#' @param agg character. Spatial aggregation level. \code{brasil} for Brazil,
#' \code{uf} for Unidade da Federação, and \code{capital} for state capitals
#' and Distrito Federal.
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
#' @examples
#' # Some examples
#' \dontrun{
#' indi_0051(agg = "brasil", ano = 2023)
#' indi_0051(agg = "uf", ano = 2023)
#' indi_0051(agg = "capital", ano = 2023)
#' indi_0051(agg = "capital", ano = c(2022, 2023))
#' }
#'
#' @export
indi_0051 <- function(
  agg,
  agg_time = "year",
  ano,
  multi = 100,
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

  # The annual Education module used here is available from 2016 onward
  if (any(ano < 2016L)) {
    stop(
      "indi_0051 currently supports PNAD Continua Education microdata from 2016 onward.",
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

    # Variable for current technical/normal course changed from 2019 onward
    if (ano_i >= 2019L) {
      technical_var <- "V3019A"
      technical_yes <- c(1, 2)
    } else {
      technical_var <- "V3019"
      technical_yes <- 1
    }

    vars <- c(
      "UF",
      "Capital",
      "V2009",
      "V3002",
      technical_var,
      "V3024",
      "V3025",
      "V3026",
      "VD4001",
      "VD4002"
    )

    # Download/read annual PNAD Continua Education microdata (2nd quarter)
    #
    # design = FALSE is intentional. The survey design is created explicitly
    # with PNADcIBGE::pnadc_design() below, so users do not need to attach
    # PNADcIBGE with library(PNADcIBGE).
    pnad <- PNADcIBGE::get_pnadc(
      year = ano_i,
      topic = 2,
      selected = FALSE,
      vars = vars,
      labels = FALSE,
      deflator = FALSE,
      design = FALSE,
      reload = reload,
      savedir = savedir
    )

    if (is.null(pnad) || nrow(pnad) == 0L) {
      stop(
        paste0(
          "PNADcIBGE did not return Education microdata for year ",
          ano_i,
          "."
        ),
        call. = FALSE
      )
    }

    # Harmonize technical/normal-course attendance across questionnaire versions
    if (technical_var == "V3019A") {
      pnad$.curso_tecnico_normal <- as.integer(
        pnad$V3019A %in% technical_yes
      )
    } else {
      pnad$.curso_tecnico_normal <- as.integer(
        pnad$V3019 %in% technical_yes
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

    # Denominator: young people aged 15 to 29 years
    pnad <- subset(
      pnad,
      V2009 >= 15 &
        V2009 <= 29
    )

    if (nrow(pnad$variables) == 0L) {
      stop(
        paste0(
          "No valid observations were found for indi_0051 in year ",
          ano_i,
          "."
        ),
        call. = FALSE
      )
    }

    # Numerator:
    # - does not attend school/basic or higher education (V3002)
    # - does not attend technical/normal course
    # - does not attend pre-university course
    # - does not attend higher-education extension/training
    # - does not attend professional qualification course
    # - is not occupied in the reference week
    #
    # VD4001 == 2 identifies people outside the labour force.
    # VD4002 == 2 identifies unemployed people.
    # Together, these groups represent young people who are not occupied.
    pnad <- stats::update(
      pnad,
      indicador = as.numeric(
        !(
          V3002 %in% 1 |
            .curso_tecnico_normal %in% 1 |
            V3024 %in% 1 |
            V3025 %in% 1 |
            V3026 %in% 1
        ) &
          (
            VD4001 %in% 2 |
              VD4002 %in% 2
          )
      )
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
          nome = "indi_0051",
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
    if (agg == "uf") {

      estimativa <- survey::svyby(
        ~indicador,
        ~UF,
        design = pnad,
        FUN = survey::svymean,
        na.rm = TRUE,
        keep.names = FALSE
      )

      return(
        tibble::tibble(
          nome = "indi_0051",
          ano = ano_i,
          agg = "uf",
          uf = estimativa$UF,
          valor = round(
            estimativa$indicador * multi,
            decimals
          )
        )
      )
    }

    # Capitals and Distrito Federal
    pnad_capital <- subset(
      pnad,
      !is.na(Capital)
    )

    estimativa <- survey::svyby(
      ~indicador,
      ~Capital,
      design = pnad_capital,
      FUN = survey::svymean,
      na.rm = TRUE,
      keep.names = FALSE
    )

    tibble::tibble(
      nome = "indi_0051",
      ano = ano_i,
      agg = "capital",
      capital = estimativa$Capital,
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
