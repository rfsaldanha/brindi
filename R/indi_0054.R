#' Indicator: Taxa de crescimento anual da população
#'
#' @param agg character. Spatial aggregation level. \code{brasil} for Brazil,
#' \code{uf_res} for Unidade da Federação, and \code{mun_res} for municipality.
#' @param agg_time character. Time aggregation level. \code{year} for yearly
#' population growth. Defaults to \code{year}.
#' @param ano numeric. Vector with exactly two years: the initial and final
#' years of the reference period, e.g. \code{c(2020, 2021)}.
#' @param multi numeric. Multiplicator for the indicator. Defaults to \code{100}.
#' @param decimals integer. Number of decimals for indicator. Defaults to \code{2}.
#'
#' @details
#' This indicator corresponds to RIPSA DEM.1.03 and measures the average annual
#' geometric growth of the resident population over a reference period.
#'
#' Population data are retrieved from IBGE SIDRA table 6579
#' (População residente estimada), variable 9324.
#'
#' The function first attempts retrieval through \pkg{sidrar}. If the SIDRA
#' values endpoint is unavailable or blocked, it automatically falls back to
#' the official IBGE Aggregates API (\code{servicodados.ibge.gov.br}), using
#' the same table and variable.
#'
#' The indicator is calculated as:
#'
#' \deqn{
#'   \left[
#'     \left(\frac{P_{t+n}}{P_t}\right)^{1/n} - 1
#'   \right] \times 100
#' }
#'
#' where \eqn{P_t} is the population in the initial year,
#' \eqn{P_{t+n}} is the population in the final year, and \eqn{n} is the
#' number of years between the two reference dates.
#'
#' Only years available in SIDRA table 6579 can be used.
#'
#' @examples
#' # Some examples
#' \dontrun{
#' indi_0054(agg = "brasil", ano = c(2020, 2021))
#' indi_0054(agg = "uf_res", ano = c(2020, 2021))
#' indi_0054(agg = "mun_res", ano = c(2020, 2021))
#' indi_0054(agg = "brasil", ano = c(2019, 2021))
#' }
#'
#' @export
indi_0054 <- function(
  agg,
  agg_time = "year",
  ano,
  multi = 100,
  decimals = 2
) {

  if (!agg %in% c("brasil", "uf_res", "mun_res")) {
    stop(
      "`agg` must be one of: 'brasil', 'uf_res', or 'mun_res'.",
      call. = FALSE
    )
  }

  if (!identical(agg_time, "year")) {
    stop(
      "Population growth is annual. Use `agg_time = 'year'`.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(ano) ||
      length(ano) != 2L ||
      any(is.na(ano))
  ) {
    stop(
      "`ano` must contain exactly two valid years: c(initial_year, final_year).",
      call. = FALSE
    )
  }

  ano <- as.integer(ano)

  if (ano[2] <= ano[1]) {
    stop(
      "The final year in `ano` must be greater than the initial year.",
      call. = FALSE
    )
  }

  n_years <- ano[2] - ano[1]

  sidra_geo <- switch(
    agg,
    brasil = "Brazil",
    uf_res = "State",
    mun_res = "City"
  )

  ibge_geo <- switch(
    agg,
    brasil = "N1[all]",
    uf_res = "N3[all]",
    mun_res = "N6[all]"
  )

  # ---- Primary source: sidrar ------------------------------------------------

  sidrar_error <- NULL

  pop <- tryCatch(
    sidrar::get_sidra(
      x = 6579,
      variable = 9324,
      period = as.character(ano),
      geo = sidra_geo,
      header = FALSE,
      format = 4,
      value_type = "numeric"
    ),
    error = function(e) {
      sidrar_error <<- conditionMessage(e)
      NULL
    }
  )

  if (!is.null(pop) && nrow(pop) > 0L) {

    required_cols <- c("D1C", "D3C", "V")

    if (all(required_cols %in% names(pop))) {
      pop <- data.frame(
        geo_code = as.character(pop$D1C),
        ano = as.integer(as.character(pop$D3C)),
        populacao = as.numeric(pop$V),
        stringsAsFactors = FALSE
      )
    } else {
      pop <- NULL
      sidrar_error <- "Unexpected structure returned by sidrar."
    }
  } else {
    pop <- NULL
  }

  # ---- Fallback: official IBGE Aggregates API -------------------------------

  if (is.null(pop)) {

    periods <- paste(ano, collapse = "%7C")

    endpoint <- paste0(
      "https://servicodados.ibge.gov.br/api/v3/agregados/",
      "6579/periodos/",
      periods,
      "/variaveis/9324"
    )

    fallback_error <- NULL

    response <- tryCatch(
      httr::GET(
        url = endpoint,
        query = list(
          localidades = ibge_geo
        ),
        httr::user_agent(
          "brindi R package - IBGE aggregate data client"
        ),
        httr::timeout(120)
      ),
      error = function(e) {
        fallback_error <<- conditionMessage(e)
        NULL
      }
    )

    if (is.null(response)) {
      stop(
        paste0(
          "Could not retrieve population data from IBGE. ",
          "sidrar error: ", sidrar_error, ". ",
          "IBGE Aggregates API error: ", fallback_error
        ),
        call. = FALSE
      )
    }

    if (httr::http_error(response)) {
      stop(
        paste0(
          "Could not retrieve population data from IBGE. ",
          "sidrar error: ", sidrar_error, ". ",
          "IBGE Aggregates API returned HTTP ",
          httr::status_code(response),
          "."
        ),
        call. = FALSE
      )
    }

    txt <- httr::content(
      response,
      as = "text",
      encoding = "UTF-8"
    )

    json <- tryCatch(
      jsonlite::fromJSON(
        txt,
        simplifyVector = FALSE
      ),
      error = function(e) {
        fallback_error <<- conditionMessage(e)
        NULL
      }
    )

    if (
      is.null(json) ||
        length(json) == 0L ||
        is.null(json[[1]]$resultados)
    ) {
      stop(
        paste0(
          "IBGE returned no valid population data for the requested period. ",
          "Check whether both years are available in SIDRA table 6579."
        ),
        call. = FALSE
      )
    }

    series <- json[[1]]$resultados[[1]]$series

    if (is.null(series) || length(series) == 0L) {
      stop(
        "IBGE returned no geographic series for the requested period.",
        call. = FALSE
      )
    }

    rows <- lapply(series, function(x) {

      values <- x$serie

      if (is.null(values)) {
        return(NULL)
      }

      years_available <- intersect(
        names(values),
        as.character(ano)
      )

      if (length(years_available) == 0L) {
        return(NULL)
      }

      data.frame(
        geo_code = as.character(x$localidade$id),
        ano = as.integer(years_available),
        populacao = suppressWarnings(
          as.numeric(
            unlist(values[years_available], use.names = FALSE)
          )
        ),
        stringsAsFactors = FALSE
      )
    })

    rows <- rows[!vapply(rows, is.null, logical(1))]

    if (length(rows) == 0L) {
      stop(
        "IBGE returned no population values for the requested years.",
        call. = FALSE
      )
    }

    pop <- do.call(
      rbind,
      rows
    )
  }

  # ---- Indicator calculation -------------------------------------------------

  pop <- pop[
    !is.na(pop$populacao) &
      pop$populacao > 0 &
      pop$ano %in% ano,
    ,
    drop = FALSE
  ]

  if (nrow(pop) == 0L) {
    stop(
      "No valid population values were returned for the requested period.",
      call. = FALSE
    )
  }

  pop_ini <- pop[
    pop$ano == ano[1],
    c("geo_code", "populacao"),
    drop = FALSE
  ]

  pop_fim <- pop[
    pop$ano == ano[2],
    c("geo_code", "populacao"),
    drop = FALSE
  ]

  names(pop_ini)[2] <- "pop_ini"
  names(pop_fim)[2] <- "pop_fim"

  dados <- merge(
    pop_ini,
    pop_fim,
    by = "geo_code",
    all = FALSE,
    sort = TRUE
  )

  if (nrow(dados) == 0L) {
    stop(
      paste0(
        "No geographic unit has valid population values in both requested ",
        "years. Check whether both years are available in SIDRA table 6579."
      ),
      call. = FALSE
    )
  }

  dados$valor <- (
    (dados$pop_fim / dados$pop_ini)^(1 / n_years) - 1
  ) * multi

  dados$valor <- round(
    dados$valor,
    decimals
  )

  if (agg == "brasil") {
    return(
      tibble::tibble(
        nome = "indi_0054",
        ano_ini = ano[1],
        ano_fim = ano[2],
        agg = "brasil",
        valor = dados$valor[1]
      )
    )
  }

  if (agg == "uf_res") {
    return(
      tibble::tibble(
        nome = "indi_0054",
        ano_ini = ano[1],
        ano_fim = ano[2],
        agg = "uf_res",
        uf_res = dados$geo_code,
        valor = dados$valor
      )
    )
  }

  tibble::tibble(
    nome = "indi_0054",
    ano_ini = ano[1],
    ano_fim = ano[2],
    agg = "mun_res",
    mun_res = dados$geo_code,
    valor = dados$valor
  )
}
