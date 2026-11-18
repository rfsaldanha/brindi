#' Indicator: Proporção de analfabetismo na população
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
#' This indicator corresponds to RIPSA SOC.1.01 and represents the percentage
#' of people aged 15 years or older who are illiterate.
#'
#' Data are retrieved from IBGE SIDRA table 7113
#' (Taxa de analfabetismo das pessoas de 15 anos ou mais de idade,
#' por sexo e grupo de idade), variable 10267.
#'
#' The function first attempts retrieval through \pkg{sidrar}. If the SIDRA
#' values endpoint is unavailable or blocked, it automatically falls back to
#' the official IBGE Aggregates API (\code{servicodados.ibge.gov.br}), using
#' the same table and variable.
#'
#' Available years in table 7113 are currently 2016, 2017, 2018, 2019,
#' 2022, 2023, 2024 and 2025.
#'
#' @examples
#' # Some examples
#' \dontrun{
#' indi_0057(agg = "brasil", ano = 2024)
#' indi_0057(agg = "brasil", ano = 2024, sexo = "feminino")
#' indi_0057(agg = "uf_res", ano = 2024)
#' indi_0057(agg = "uf_res", ano = c(2023, 2024))
#' }
#'
#' @export
indi_0057 <- function(
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
      "Illiteracy proportion is annual. Use `agg_time = 'year'`.",
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
        "indi_0057 currently supports SIDRA table 7113 for years: ",
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

  # ---- Primary source: sidrar ------------------------------------------------

  sidrar_error <- NULL

  raw <- tryCatch(
    sidrar::get_sidra(
      x = 7113,
      variable = 10267,
      period = as.character(ano),
      geo = sidra_geo,
      classific = "all",
      category = "all",
      header = FALSE,
      format = 4,
      value_type = "numeric"
    ),
    error = function(e) {
      sidrar_error <<- conditionMessage(e)
      NULL
    }
  )

  dados <- NULL

  if (!is.null(raw) && nrow(raw) > 0L) {

    required_cols <- c("D1C", "D3C", "V")

    if (all(required_cols %in% names(raw))) {

      name_cols <- grep(
        "^D[0-9]+N$",
        names(raw),
        value = TRUE
      )

      sex_col <- name_cols[
        vapply(
          name_cols,
          function(z) {
            any(
              as.character(raw[[z]]) %in%
                c("Total", "Homens", "Mulheres"),
              na.rm = TRUE
            )
          },
          logical(1)
        )
      ][1]

      age_col <- name_cols[
        vapply(
          name_cols,
          function(z) {
            any(
              grepl(
                "^15 anos ou mais",
                as.character(raw[[z]]),
                ignore.case = TRUE
              ),
              na.rm = TRUE
            )
          },
          logical(1)
        )
      ][1]

      if (!is.na(sex_col) && !is.na(age_col)) {

        keep <- as.character(raw[[sex_col]]) == sexo_sidra &
          grepl(
            "^15 anos ou mais",
            as.character(raw[[age_col]]),
            ignore.case = TRUE
          )

        raw2 <- raw[
          keep,
          ,
          drop = FALSE
        ]

        if (nrow(raw2) > 0L) {
          dados <- data.frame(
            geo_code = as.character(raw2$D1C),
            ano = as.integer(as.character(raw2$D3C)),
            valor = as.numeric(raw2$V),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    if (is.null(dados)) {
      sidrar_error <- "Unexpected classification structure returned by sidrar."
    }
  }

  # ---- Fallback: official IBGE Aggregates API -------------------------------

  if (is.null(dados)) {

    periods <- paste(ano, collapse = "%7C")

    endpoint <- paste0(
      "https://servicodados.ibge.gov.br/api/v3/agregados/",
      "7113/periodos/",
      periods,
      "/variaveis/10267"
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
          "Could not retrieve illiteracy data from IBGE. ",
          "sidrar error: ", sidrar_error, ". ",
          "IBGE Aggregates API error: ", fallback_error
        ),
        call. = FALSE
      )
    }

    if (httr::http_error(response)) {
      stop(
        paste0(
          "Could not retrieve illiteracy data from IBGE. ",
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
        "IBGE returned no valid illiteracy data for the requested period.",
        call. = FALSE
      )
    }

    rows <- list()

    # Normalize category labels because the IBGE API may use names such as
    # "Sexo das pessoas" / "Grupo de idade das pessoas" instead of the
    # shorter labels used in SIDRA's visual interface.
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

    sex_match <- function(labels, sexo) {

      labels <- normalize_label(labels)

      if (sexo == "total") {
        return(any(labels == "total"))
      }

      if (sexo == "masculino") {
        return(
          any(
            grepl(
              "^(homem|homens|masculino|masculinos)$",
              labels
            )
          )
        )
      }

      any(
        grepl(
          "^(mulher|mulheres|feminino|femininos)$",
          labels
        )
      )
    }

    age_match <- function(labels) {

      labels <- normalize_label(labels)

      any(
        grepl(
          "^15.*anos.*mais",
          labels
        )
      )
    }

    for (resultado in json[[1]]$resultados) {

      classes <- resultado$classificacoes

      if (is.null(classes) || length(classes) == 0L) {
        next
      }

      # Read all category labels independently of the classification names.
      class_labels <- unlist(
        lapply(
          classes,
          function(x) {
            if (is.null(x$categoria)) {
              return(character(0))
            }

            as.character(
              unname(
                unlist(
                  x$categoria,
                  use.names = FALSE
                )
              )
            )
          }
        ),
        use.names = FALSE
      )

      if (
        length(class_labels) == 0L ||
          !sex_match(class_labels, sexo) ||
          !age_match(class_labels)
      ) {
        next
      }

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
          "IBGE returned no matching records for sex '",
          sexo,
          "' and the population aged 15 years or older."
        ),
        call. = FALSE
      )
    }

    dados <- do.call(
      rbind,
      rows
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
      "No valid illiteracy values were returned for the requested period.",
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
        "Illiteracy values were not found for all requested Brazil years.",
        call. = FALSE
      )
    }

    return(
      tibble::tibble(
        nome = "indi_0057",
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
    nome = "indi_0057",
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
