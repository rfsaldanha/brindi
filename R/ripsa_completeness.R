#' RIPSA "Livro Verde" indicator matrix
#'
#' Loads the reference table of indicators from the RIPSA "Livro Verde"
#' (Indicadores e Dados Basicos para a Saude no Brasil), organized by
#' dimension: \code{DEM} (Demograficos), \code{SOC} (Socioeconomicos),
#' \code{MRT} (Mortalidade), \code{MRB} (Morbidade), \code{FRP} (Fatores
#' de risco e protecao), \code{REC} (Recursos) and \code{COB} (Cobertura).
#' The table was extracted from the official summary (sumario) of the
#' Livro Verde PDF and bundled with the package.
#'
#' @return A tibble with columns \code{code}, \code{prefixo},
#'   \code{dimensao}, \code{dim}, \code{num}, \code{sub_num} and
#'   \code{nome}. \code{sub_num} is not \code{NA} for indicators that are
#'   split into sub-variants by data source (e.g. \code{FRP.2.01.1} and
#'   \code{FRP.2.01.2} for the same indicator measured by Vigitel and PNS).
#'
#' @examples
#' ripsa_matrix()
#'
#' @export
ripsa_matrix <- function() {
  path <- system.file("extdata", "ripsa_livro_verde_indicadores.csv", package = "brindi")
  res <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  tibble::as_tibble(res)
}

#' BRINDI indicator catalog
#'
#' Loads the catalog of \code{indi_*} functions implemented in the package,
#' with the indicator title (from the function documentation) and the main
#' data source used to compute the numerator.
#'
#' @return A tibble with columns \code{funcao}, \code{titulo}, \code{fonte}
#'   and \code{multiplicador}.
#'
#' @examples
#' brindi_indicator_catalog()
#'
#' @export
brindi_indicator_catalog <- function() {
  path <- system.file("extdata", "brindi_indicadores_catalogo.csv", package = "brindi")
  res <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  tibble::as_tibble(res)
}

#' Tokenize an indicator title for comparison
#'
#' Internal helper. Lowercases, strips accents/punctuation and removes
#' generic epidemiological "template" words (e.g. "taxa", "mortalidade",
#' "incidencia") that are shared by nearly every indicator title and would
#' otherwise inflate similarity scores between unrelated indicators. Also
#' normalizes a few number words (e.g. "cinco" to "5") that commonly differ
#' in wording between the RIPSA and BRINDI titles for the same indicator.
#'
#' @param x character vector of titles.
#' @return a list of character vectors of tokens, one per element of \code{x}.
#' @noRd
ripsa_tokenize <- function(x) {
  stopwords <- c(
    "de", "da", "do", "das", "dos", "a", "o", "e", "em", "por", "para",
    "com", "no", "na", "nos", "nas", "ao", "aos", "as", "os", "que",
    "sobre", "entre", "sua", "seu", "ou", "todas",
    "taxa", "taxas", "bruta", "bruto", "especifica", "especifico",
    "proporcao", "proporcional", "razao", "indice", "numero",
    "incidencia", "prevalencia", "mortalidade", "cobertura",
    "internacoes", "internacao", "hospitalares", "residente",
    "residentes", "populacao", "idade", "anos", "ano", "geografico",
    "espaco", "determinado", "total"
  )
  num_words <- c(
    um = "1", uma = "1", cinco = "5", dez = "10",
    quinze = "15", sessenta = "60", dezenove = "19"
  )

  x <- tolower(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9 ]", " ", x)

  lapply(strsplit(x, "\\s+"), function(toks) {
    toks <- toks[nchar(toks) > 0]
    toks <- ifelse(toks %in% names(num_words), num_words[toks], toks)
    toks[!toks %in% stopwords]
  })
}

#' Jaccard similarity between token sets
#' @param a,b character vectors of tokens.
#' @return numeric similarity between 0 and 1.
#' @noRd
ripsa_jaccard <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(0)
  length(intersect(a, b)) / length(union(a, b))
}

#' Match BRINDI indicators to the RIPSA Livro Verde matrix
#'
#' Identifies, for each indicator in the RIPSA Livro Verde matrix, whether
#' there is a corresponding \code{indi_*} function implemented in the
#' package. The match is done by token overlap (Jaccard similarity) between
#' the RIPSA indicator name and the BRINDI indicator title, after removing
#' generic epidemiological terms shared by most titles (see
#' \code{ripsa_tokenize}), since there is no direct code mapping between
#' the two.
#'
#' @param threshold numeric. Minimum Jaccard similarity (0 = no shared
#'   tokens, 1 = identical) to consider a match. Defaults to \code{0.55},
#'   chosen so that only strong, specific matches (e.g. same disease/cause)
#'   are accepted and generic template overlaps are rejected.
#' @param show_all logical. If TRUE, shows all matches with similarity scores,
#'   including those below threshold as "sugestivos". Defaults to FALSE.
#'
#' @return A tibble with all RIPSA indicators, the best matching BRINDI
#'   function (if any), the similarity score, a logical \code{implementado}
#'   column, and a \code{status} column ("confirmado", "sugerivo", "rejeitado").
#'
#' @examples
#' ripsa_match_indicators()
#' ripsa_match_indicators(show_all = TRUE)
#'
#' @export
ripsa_match_indicators <- function(threshold = 0.55, show_all = FALSE) {
  ripsa <- ripsa_matrix()
  catalog <- brindi_indicator_catalog()

  ripsa_tokens <- ripsa_tokenize(ripsa$nome)
  catalog_tokens <- ripsa_tokenize(catalog$titulo)

  funcao <- character(nrow(ripsa))
  titulo_brindi <- character(nrow(ripsa))
  fonte <- character(nrow(ripsa))
  score <- numeric(nrow(ripsa))

  for (i in seq_len(nrow(ripsa))) {
    scores <- vapply(catalog_tokens, ripsa_jaccard, numeric(1), b = ripsa_tokens[[i]])
    idx <- which.max(scores)
    funcao[i] <- catalog$funcao[idx]
    titulo_brindi[i] <- catalog$titulo[idx]
    fonte[i] <- catalog$fonte[idx]
    score[i] <- scores[idx]
  }

  ripsa$funcao <- funcao
  ripsa$titulo_brindi <- titulo_brindi
  ripsa$fonte <- fonte
  ripsa$similaridade <- round(score, 3)
  ripsa$implementado <- ripsa$similaridade >= threshold

  # Status column for manual verification
  ripsa$status <- dplyr::case_when(
    ripsa$implementado ~ "confirmado",
    ripsa$similaridade >= 0.3 ~ "sugerivo",
    TRUE ~ "rejeitado"
  )

  if (!show_all) {
    ripsa$funcao[!ripsa$implementado] <- NA_character_
    ripsa$titulo_brindi[!ripsa$implementado] <- NA_character_
    ripsa$fonte[!ripsa$implementado] <- NA_character_
    ripsa$status[!ripsa$implementado] <- "rejeitado"
  }

  tibble::as_tibble(ripsa)
}

#' RIPSA completeness summary by dimension
#'
#' Summarizes, for each RIPSA dimension, how many indicators are already
#' implemented in the package as \code{indi_*} functions.
#'
#' @param threshold numeric. Passed to \code{\link{ripsa_match_indicators}}.
#'   Defaults to \code{0.55}.
#'
#' @return A tibble with columns \code{dimensao}, \code{total},
#'   \code{implementados}, \code{pendentes} and \code{percentual}.
#'
#' @examples
#' ripsa_completeness_summary()
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
ripsa_completeness_summary <- function(threshold = 0.55) {
  matched <- ripsa_match_indicators(threshold = threshold)

  matched %>%
    dplyr::group_by(.data$dimensao) %>%
    dplyr::summarise(
      total = dplyr::n(),
      implementados = sum(.data$implementado),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pendentes = .data$total - .data$implementados,
      percentual = round(100 * .data$implementados / .data$total, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$percentual))
}
