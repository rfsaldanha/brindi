#' Indicator: Esperança de vida aos 60 anos de idade
#'
#' @param agg character. Spatial aggregation level. \code{brasil} for Brazil
#' and \code{uf_res} for Unidade da Federação.
#' @param agg_time character. Time aggregation level. \code{year} for yearly
#' data. Defaults to \code{year}.
#' @param ano numeric. Reference year or vector of years.
#' @param sexo character. Sex category. One of \code{total},
#' \code{masculino}, or \code{feminino}. Defaults to \code{total}.
#' @param decimals integer. Number of decimals for indicator. Defaults to \code{1}.
#' @param reload logical. Whether the IBGE projections workbook should be
#' downloaded again when already available in cache. Defaults to \code{FALSE}.
#' @param savedir character. Directory used to cache the IBGE projections
#' workbook. Defaults to the BRINDI user cache outside the package repository.
#'
#' @details
#' This indicator corresponds to RIPSA DEM.3.05 and represents the average
#' number of additional years a person aged 60 is expected to live if the
#' mortality conditions observed in the reference population remain constant.
#'
#' The function uses the official IBGE Population Projections, Revision 2024,
#' workbook "Indicadores Implícitos", covering Brazil and the Federation Units
#' for 2000-2070.
#'
#' The workbook is structured as a long table containing year, geographic code,
#' acronym, locality and demographic indicators. Life expectancy at age 60 is
#' identified from the e60 columns for total, men and women.
#'
#' @examples
#' # Some examples
#' \dontrun{
#' indi_0056(agg = "brasil", ano = 2024)
#' indi_0056(agg = "brasil", ano = 2024, sexo = "feminino")
#' indi_0056(agg = "uf_res", ano = 2024)
#' indi_0056(agg = "uf_res", ano = c(2023, 2024))
#' }
#'
#' @export
indi_0056 <- function(
  agg,
  agg_time = "year",
  ano,
  sexo = "total",
  decimals = 1,
  reload = FALSE,
  savedir = file.path(tools::R_user_dir("brindi", "cache"), "ibge")
) {

  if (!agg %in% c("brasil", "uf_res")) {
    stop(
      "`agg` must be either 'brasil' or 'uf_res'.",
      call. = FALSE
    )
  }

  if (!identical(agg_time, "year")) {
    stop(
      "Life expectancy at age 60 is annual. Use `agg_time = 'year'`.",
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

  if (any(ano < 2000L | ano > 2070L)) {
    stop(
      "indi_0056 supports years from 2000 to 2070 in the IBGE Revision 2024 projections.",
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

  if (!dir.exists(savedir)) {
    dir.create(
      savedir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  url <- paste0(
    "https://ftp.ibge.gov.br/Projecao_da_Populacao/",
    "Projecao_da_Populacao_2024/",
    "projecoes_2024_tab4_indicadores.xlsx"
  )

  destfile <- file.path(
    savedir,
    "projecoes_2024_tab4_indicadores.xlsx"
  )

  if (reload || !file.exists(destfile)) {

    download_error <- NULL

    ok <- tryCatch(
      {
        utils::download.file(
          url = url,
          destfile = destfile,
          mode = "wb",
          quiet = TRUE
        )
        TRUE
      },
      error = function(e) {
        download_error <<- conditionMessage(e)
        FALSE
      }
    )

    if (!ok || !file.exists(destfile) || file.info(destfile)$size == 0) {
      stop(
        paste0(
          "Could not download the IBGE Population Projections Revision 2024 ",
          "workbook.",
          if (!is.null(download_error)) {
            paste0(" Original error: ", download_error)
          } else {
            ""
          }
        ),
        call. = FALSE
      )
    }
  }

  normalize_text <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- iconv(
      x,
      from = "",
      to = "ASCII//TRANSLIT"
    )
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x
  }

  make_unique_names <- function(x) {
    x[x == ""] <- "x"
    make.unique(x, sep = "_")
  }

  parse_workbook <- function() {

    sheets <- readxl::excel_sheets(destfile)

    for (sheet in sheets) {

      raw <- suppressMessages(
        readxl::read_excel(
          path = destfile,
          sheet = sheet,
          col_names = FALSE,
          col_types = "text"
        )
      )

      if (nrow(raw) < 2L || ncol(raw) < 4L) {
        next
      }

      mat <- as.data.frame(
        raw,
        stringsAsFactors = FALSE
      )

      header_row <- NA_integer_

      for (i in seq_len(min(nrow(mat), 30L))) {

        row_norm <- normalize_text(
          unlist(
            mat[i, ],
            use.names = FALSE
          )
        )

        has_year <- any(row_norm == "ano")
        has_sigla <- any(row_norm == "sigla")
        has_local <- any(row_norm %in% c("local", "localidade"))

        if (has_year && has_sigla && has_local) {
          header_row <- i
          break
        }
      }

      if (is.na(header_row)) {
        next
      }

      headers <- normalize_text(
        unlist(
          mat[header_row, ],
          use.names = FALSE
        )
      )

      headers <- make_unique_names(headers)

      dat <- mat[
        (header_row + 1L):nrow(mat),
        ,
        drop = FALSE
      ]

      names(dat) <- headers

      year_col <- names(dat)[
        names(dat) == "ano"
      ][1]

      sigla_col <- names(dat)[
        names(dat) == "sigla"
      ][1]

      local_col <- names(dat)[
        names(dat) %in% c("local", "localidade")
      ][1]

      code_col <- names(dat)[
        grepl("^cod", names(dat))
      ][1]

      if (
        any(
          is.na(
            c(
              year_col,
              sigla_col,
              local_col,
              code_col
            )
          )
        )
      ) {
        next
      }

      nms <- names(dat)

      e60_total <- nms[
        grepl(
          "^e60_?t$|esperanca.*vida.*60.*(total|ambos)",
          nms
        )
      ][1]

      e60_male <- nms[
        grepl(
          "^e60_?h$|esperanca.*vida.*60.*(homem|mascul)",
          nms
        )
      ][1]

      e60_female <- nms[
        grepl(
          "^e60_?m$|esperanca.*vida.*60.*(mulher|femin)",
          nms
        )
      ][1]

      if (
        any(
          is.na(
            c(
              e60_total,
              e60_male,
              e60_female
            )
          )
        )
      ) {
        next
      }

      to_numeric <- function(x) {
        x <- trimws(as.character(x))
        x[x %in% c("", "-", "...", "..")] <- NA_character_
        x <- gsub(",", ".", x, fixed = TRUE)
        suppressWarnings(
          as.numeric(x)
        )
      }

      out <- data.frame(
        ano = suppressWarnings(
          as.integer(
            trimws(
              as.character(dat[[year_col]])
            )
          )
        ),
        codigo = trimws(
          as.character(dat[[code_col]])
        ),
        sigla = toupper(
          trimws(
            as.character(dat[[sigla_col]])
          )
        ),
        local = trimws(
          as.character(dat[[local_col]])
        ),
        total = to_numeric(
          dat[[e60_total]]
        ),
        masculino = to_numeric(
          dat[[e60_male]]
        ),
        feminino = to_numeric(
          dat[[e60_female]]
        ),
        stringsAsFactors = FALSE
      )

      out <- out[
        !is.na(out$ano) &
          nzchar(out$sigla),
        ,
        drop = FALSE
      ]

      if (nrow(out) > 0L) {
        return(out)
      }
    }

    stop(
      paste0(
        "Could not identify the long indicators table or the life expectancy ",
        "at age 60 columns in the IBGE Revision 2024 workbook."
      ),
      call. = FALSE
    )
  }

  dados <- parse_workbook()

  value_col <- switch(
    sexo,
    total = "total",
    masculino = "masculino",
    feminino = "feminino"
  )

  if (agg == "brasil") {

    res <- dados[
      dados$sigla == "BR" &
        dados$ano %in% ano,
      c(
        "ano",
        value_col
      ),
      drop = FALSE
    ]

    if (nrow(res) != length(ano)) {
      stop(
        "Life expectancy at age 60 values were not found for all requested Brazil years.",
        call. = FALSE
      )
    }

    res <- res[
      match(
        ano,
        res$ano
      ),
      ,
      drop = FALSE
    ]

    return(
      tibble::tibble(
        nome = "indi_0056",
        ano = res$ano,
        agg = "brasil",
        sexo = sexo,
        valor = round(
          res[[value_col]],
          decimals
        )
      )
    )
  }

  uf_siglas <- c(
    "RO", "AC", "AM", "RR", "PA", "AP", "TO",
    "MA", "PI", "CE", "RN", "PB", "PE", "AL",
    "SE", "BA", "MG", "ES", "RJ", "SP", "PR",
    "SC", "RS", "MS", "MT", "GO", "DF"
  )

  uf_codes <- c(
    RO = "11", AC = "12", AM = "13", RR = "14",
    PA = "15", AP = "16", TO = "17", MA = "21",
    PI = "22", CE = "23", RN = "24", PB = "25",
    PE = "26", AL = "27", SE = "28", BA = "29",
    MG = "31", ES = "32", RJ = "33", SP = "35",
    PR = "41", SC = "42", RS = "43", MS = "50",
    MT = "51", GO = "52", DF = "53"
  )

  res <- dados[
    dados$sigla %in% uf_siglas &
      dados$ano %in% ano,
    c(
      "ano",
      "sigla",
      value_col
    ),
    drop = FALSE
  ]

  if (nrow(res) == 0L) {
    stop(
      "No UF life expectancy at age 60 values were found for the requested years.",
      call. = FALSE
    )
  }

  res$uf_res <- unname(
    uf_codes[
      res$sigla
    ]
  )

  res <- res[
    order(
      res$ano,
      as.integer(res$uf_res)
    ),
    ,
    drop = FALSE
  ]

  expected_rows <- length(ano) * 27L

  if (nrow(res) != expected_rows) {
    stop(
      paste0(
        "Expected ",
        expected_rows,
        " UF-year observations, but found ",
        nrow(res),
        "."
      ),
      call. = FALSE
    )
  }

  tibble::tibble(
    nome = "indi_0056",
    ano = res$ano,
    agg = "uf_res",
    uf_res = res$uf_res,
    sexo = sexo,
    valor = round(
      res[[value_col]],
      decimals
    )
  )
}
