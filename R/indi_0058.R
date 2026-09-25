#' Indicator: Proporção da população sem educação básica
#'
#' @param agg character. Spatial aggregation level. \code{brasil} for Brazil
#' and \code{uf_res} for Unidade da Federação.
#' @param agg_time character. Time aggregation level. \code{year} for yearly
#' data. Defaults to \code{year}.
#' @param ano numeric. Reference year or vector of years.
#' @param sexo character. Sex category. One of \code{total},
#' \code{masculino}, or \code{feminino}. Defaults to \code{total}.
#' @param decimals integer. Number of decimals for indicator. Defaults to \code{1}.
#'
#' @details
#' This indicator corresponds to RIPSA SOC.1.02 and represents the percentage
#' of people aged 25 years or older who have not completed basic education.
#'
#' A person is considered without basic education when they have not completed
#' high school or an equivalent level.
#'
#' Data are retrieved from IBGE SIDRA table 7269
#' (Pessoas de 25 anos ou mais de idade, por sexo e grupamentos de nível de
#' instrução), variable 10270.
#'
#' The indicator is calculated as the sum of the distribution percentages for:
#' \itemize{
#'   \item Sem instrução e fundamental incompleto;
#'   \item Fundamental completo e médio incompleto.
#' }
#'
#' The function first attempts retrieval through \pkg{sidrar}. If the SIDRA
#' values endpoint is unavailable or blocked, it automatically falls back to
#' the official IBGE Aggregates API (\code{servicodados.ibge.gov.br}), using
#' the same table and variable.
#'
#' Available years in table 7269 are currently 2016, 2017, 2018, 2019,
#' 2022, 2023, 2024 and 2025.
#'
#' @examples
#' # Some examples
#' \dontrun{
#' indi_0058(agg = "brasil", ano = 2024)
#' indi_0058(agg = "brasil", ano = 2024, sexo = "feminino")
#' indi_0058(agg = "uf_res", ano = 2024)
#' indi_0058(agg = "uf_res", ano = c(2023, 2024))
#' }
#'
#' @export
indi_0058 <- function(
  agg,
  agg_time = "year",
  ano,
  sexo = "total",
  decimals = 1
) {

  if (!agg %in% c("brasil", "uf_res")) {
    stop(
      "`agg` must be either 'brasil' or 'uf_res'.",
      call. = FALSE
    )
  }

  if (!identical(agg_time, "year")) {
    stop(
      "Proportion without basic education is annual. Use `agg_time = 'year'`.",
      call. = FALSE
    )
  }

  if (!is.numeric(ano) || length(ano) < 1L || any(is.na(ano))) {
    stop(
      "`ano` must be a valid year or vector of years.",
      call. = FALSE
    )
  }

  ano <- as.integer(ano)

  valid_years <- c(
    2016L, 2017L, 2018L, 2019L,
    2022L, 2023L, 2024L, 2025L
  )

  if (any(!ano %in% valid_years)) {
    stop(
      paste0(
        "indi_0058 currently supports SIDRA table 7269 for years: ",
        paste(valid_years, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (
    length(sexo) != 1L ||
      !sexo %in% c("total", "masculino", "feminino")
  ) {
    stop(
      "`sexo` must be one of: 'total', 'masculino', or 'feminino'.",
      call. = FALSE
    )
  }

  sexo_sidra <- switch(
    sexo,
    total = "Total",
    masculino = "Homens",
    feminino = "Mulheres"
  )

  sexo_id <- switch(
    sexo,
    total = 0,
    masculino = 4,
    feminino = 5
  )

  edu_ids <- c(11627, 48134)

  sidra_geo <- switch(
    agg,
    brasil = "Brazil",
    uf_res = "State"
  )

  ibge_geo <- switch(
    agg,
    brasil = "N1[all]",
    uf_res = "N3[all]"
  )

  normalize_label <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- iconv(
      x,
      from = "",
      to = "ASCII//TRANSLIT"
    )
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", " ", x)
    trimws(x)
  }

  sexo_sidra_norm <- normalize_label(sexo_sidra)

  # The SIDRA labels may include the suffix "ou equivalente".
  # Match the substantive education groups instead of requiring exact labels.
  is_target_education <- function(x) {
    x <- normalize_label(x)

    grepl(
      "^sem instrucao.*fundamental incompleto",
      x
    ) |
      grepl(
        "^fundamental completo.*medio incompleto",
        x
      )
  }

  # ---- Primary source: sidrar ------------------------------------------------

  sidrar_error <- NULL
  dados <- NULL

  raw <- tryCatch(
    sidrar::get_sidra(
      x = 7269,
      variable = 10270,
      period = as.character(ano),
      geo = sidra_geo,
      classific = c("c2", "c1568"),
      category = list(sexo_id, edu_ids),
      header = FALSE,
      format = 4,
      value_type = "numeric"
    ),
    error = function(e) {
      sidrar_error <<- conditionMessage(e)
      NULL
    }
  )

  if (!is.null(raw) && nrow(raw) > 0L) {

    required_cols <- c("D1C", "D3C", "V")

    if (all(required_cols %in% names(raw))) {

      tmp <- data.frame(
        geo_code = as.character(raw$D1C),
        ano = as.integer(as.character(raw$D3C)),
        valor = suppressWarnings(
          as.numeric(raw$V)
        ),
        stringsAsFactors = FALSE
      )

      tmp <- tmp[
        !is.na(tmp$valor),
        ,
        drop = FALSE
      ]

      if (nrow(tmp) > 0L) {
        dados <- stats::aggregate(
          valor ~ geo_code + ano,
          data = tmp,
          FUN = sum
        )
      }
    }

    if (is.null(dados)) {
      sidrar_error <- "Unexpected structure returned by sidrar."
    }
  }

  # ---- Fallback: official IBGE Aggregates API -------------------------------

  if (is.null(dados)) {

    periods <- paste(ano, collapse = "%7C")

    endpoint <- paste0(
      "https://servicodados.ibge.gov.br/api/v3/agregados/",
      "7269/periodos/",
      periods,
      "/variaveis/10270"
    )

    fallback_error <- NULL

    response <- tryCatch(
      httr::GET(
        url = endpoint,
        query = list(
          localidades = ibge_geo,
          classificacao = paste0(
            "2[",
            sexo_id,
            "]|1568[",
            paste(edu_ids, collapse = ","),
            "]"
          )
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
          "Could not retrieve education data from IBGE. ",
          "sidrar error: ", sidrar_error, ". ",
          "IBGE Aggregates API error: ", fallback_error
        ),
        call. = FALSE
      )
    }

    if (httr::http_error(response)) {
      stop(
        paste0(
          "Could not retrieve education data from IBGE. ",
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
        "IBGE returned no valid education data for the requested period.",
        call. = FALSE
      )
    }

    rows <- list()

    # The API request already filters the exact SIDRA category IDs:
    # Sexo: 2[0|4|5]
    # Nível de instrução: 1568[11627,48134]
    # Therefore no text matching is needed here.
    for (resultado in json[[1]]$resultados) {

      series <- resultado$series

      if (is.null(series) || length(series) == 0L) {
        next
      }

      for (serie in series) {

        values <- serie$serie

        if (is.null(values)) {
          next
        }

        years_available <- intersect(
          names(values),
          as.character(ano)
        )

        if (length(years_available) == 0L) {
          next
        }

        values_chr <- unlist(
          values[years_available],
          use.names = FALSE
        )

        values_num <- suppressWarnings(
          as.numeric(
            gsub(
              ",",
              ".",
              as.character(values_chr),
              fixed = TRUE
            )
          )
        )

        rows[[length(rows) + 1L]] <- data.frame(
          geo_code = as.character(
            serie$localidade$id
          ),
          ano = as.integer(
            years_available
          ),
          valor = values_num,
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(rows) == 0L) {
      stop(
        paste0(
          "IBGE returned no matching records for the requested sex and ",
          "education categories (SIDRA 2[", sexo_id,
          "] and 1568[11627,48134])."
        ),
        call. = FALSE
      )
    }

    tmp <- do.call(
      rbind,
      rows
    )

    tmp <- tmp[
      !is.na(tmp$valor),
      ,
      drop = FALSE
    ]

    dados <- stats::aggregate(
      valor ~ geo_code + ano,
      data = tmp,
      FUN = sum
    )
  }

  # ---- Final formatting ------------------------------------------------------

  dados <- dados[
    !is.na(dados$ano) &
      !is.na(dados$valor) &
      dados$ano %in% ano,
    ,
    drop = FALSE
  ]

  if (nrow(dados) == 0L) {
    stop(
      "No valid values were returned for indi_0058.",
      call. = FALSE
    )
  }

  dados <- dados[
    order(
      dados$ano,
      dados$geo_code
    ),
    ,
    drop = FALSE
  ]

  if (agg == "brasil") {

    if (nrow(dados) != length(ano)) {
      stop(
        "Values were not found for all requested Brazil years.",
        call. = FALSE
      )
    }

    return(
      tibble::tibble(
        nome = "indi_0058",
        ano = dados$ano,
        agg = "brasil",
        sexo = sexo,
        valor = round(
          dados$valor,
          decimals
        )
      )
    )
  }

  expected_rows <- length(ano) * 27L

  if (nrow(dados) != expected_rows) {
    stop(
      paste0(
        "Expected ",
        expected_rows,
        " UF-year observations, but found ",
        nrow(dados),
        "."
      ),
      call. = FALSE
    )
  }

  tibble::tibble(
    nome = "indi_0058",
    ano = dados$ano,
    agg = "uf_res",
    uf_res = dados$geo_code,
    sexo = sexo,
    valor = round(
      dados$valor,
      decimals
    )
  )
}
