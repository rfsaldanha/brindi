.brindi_pcdas_query_request <- function(request_body, pcdas_token = NULL,
                                        throttle_rate = 1, max_tries = 10) {
  if (is.null(pcdas_token)) {
    pcdas_token <- rpcdas::get_pcdas_token_renviron()
  }

  more <- TRUE
  content <- list()
  pcdas_url <- get("pcdas_url", envir = asNamespace("rpcdas"))

  while (isTRUE(more)) {
    req <- httr2::request(pcdas_url) %>%
      httr2::req_url_path_append("sql_query") %>%
      httr2::req_body_json(request_body, auto_unbox = TRUE) %>%
      httr2::req_throttle(throttle_rate, realm = pcdas_url) %>%
      httr2::req_retry(max_tries = max_tries)

    resp <- httr2::req_perform(req = req)
    tmp_content <- httr2::resp_body_json(resp)

    if (length(tmp_content$rows) > 0) {
      content <- append(content, list(tmp_content))
    }

    if (length(tmp_content$rows) > 0 &&
        !is.null(content[[length(content)]]$cursor)) {
      request_body <- list(
        token = list(token = pcdas_token),
        sql = list(sql = list(cursor = content[[length(content)]]$cursor))
      )
    } else {
      more <- FALSE
    }
  }

  content
}

.brindi_convert_content_to_df <- function(content) {
  rows <- lapply(content$rows, unlist)
  res <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(res) <- vapply(content$columns, `[[`, character(1), "name")
  res
}

.brindi_convert_list_content_to_df <- function(list_content) {
  tmp <- lapply(list_content, .brindi_convert_content_to_df)
  tmp <- lapply(tmp, `names<-`, names(tmp[[1]]))
  as.data.frame(do.call(rbind, tmp))
}

.brindi_pcdas_agg_geo <- function(agg, source) {
  if (source == "sim") {
    switch(
      agg,
      uf_res = "res_CODIGO_UF",
      uf_ocor = "ocor_CODIGO_UF",
      mun_res = "res_codigo_adotado",
      mun_ocor = "ocor_codigo_adotado",
      regsaude_res = "res_RSAUDCOD",
      regsaude_ocor = "ocor_RSAUDCOD",
      regsaude_449_res = "res_codigo_adotado",
      regsaude_449_ocor = "ocor_codigo_adotado"
    )
  } else if (source == "sinasc") {
    switch(
      agg,
      uf_res = "res_CODIGO_UF",
      uf_ocor = "nasc_CODIGO_UF",
      mun_res = "res_codigo_adotado",
      mun_ocor = "nasc_codigo_adotado",
      regsaude_res = "res_RSAUDCOD",
      regsaude_ocor = "nasc_RSAUDCOD",
      regsaude_449_res = "res_codigo_adotado",
      regsaude_449_ocor = "nasc_codigo_adotado"
    )
  } else if (source == "sih") {
    switch(
      agg,
      uf_res = "res_CODIGO_UF",
      uf_ocor = "int_CODIGO_UF",
      mun_res = "res_codigo_adotado",
      mun_ocor = "int_codigo_adotado",
      regsaude_res = "res_RSAUDCOD",
      regsaude_ocor = "int_RSAUDCOD",
      regsaude_449_res = "res_codigo_adotado",
      regsaude_449_ocor = "int_codigo_adotado"
    )
  }
}

.brindi_get_sim <- function(agg, agg_time = "year", ano, pcdas_token = NULL,
                            sexo = NULL, idade_a = NULL, idade_b = NULL,
                            cid_like = NULL, cid_in = NULL,
                            more_filters = NULL, fetch_size = 65000) {
  if (is.null(pcdas_token)) {
    pcdas_token <- rpcdas::get_pcdas_token_renviron()
  }

  agg_geo <- .brindi_pcdas_agg_geo(agg, "sim")
  sql_select <- glue::glue("SELECT {agg_geo} AS agg, COUNT(1) AS freq")
  sql_from <- 'FROM "datasus-sim"'
  sql_where <- glue::glue("WHERE ano_obito IN ({glue::glue_collapse(ano, sep = ', ')})")
  sql_group_by <- glue::glue("GROUP BY {agg_geo}")

  if (agg_time == "year") {
    sql_select <- glue::glue(sql_select, ", DATETIME_FORMAT(data_obito, 'yyyy') AS agg_time")
    sql_group_by <- glue::glue(sql_group_by, ", agg_time")
  } else if (agg_time == "month") {
    sql_select <- glue::glue(sql_select, ", DATETIME_FORMAT(data_obito, 'yyyy-MM') AS agg_time")
    sql_group_by <- glue::glue(sql_group_by, ", agg_time")
  } else if (agg_time == "week") {
    sql_select <- glue::glue(sql_select, ", DATETIME_FORMAT(data_obito, 'yyyy-ww') AS agg_time")
    sql_group_by <- glue::glue(sql_group_by, ", agg_time")
  }

  if (!is.null(sexo)) {
    sql_where <- glue::glue(sql_where, "AND def_sexo = '{sexo}'", .sep = " ")
  }
  if (!is.null(idade_a) && is.null(idade_b)) {
    sql_where <- glue::glue(sql_where, "AND idade_obito_anos <= '{idade_a}'", .sep = " ")
  }
  if (!is.null(idade_b) && is.null(idade_a)) {
    sql_where <- glue::glue(sql_where, "AND idade_obito_anos >= '{idade_b}'", .sep = " ")
  }
  if (!is.null(idade_a) && !is.null(idade_b)) {
    sql_where <- glue::glue(
      sql_where,
      "AND idade_obito_anos >= '{idade_a}' AND idade_obito_anos <= '{idade_b}'",
      .sep = " "
    )
  }
  if (!is.null(cid_like)) {
    sql_where <- glue::glue(sql_where, "AND CAUSABAS LIKE '{cid_like}%'", .sep = " ")
  }
  if (!is.null(cid_in)) {
    sl <- stringr::str_replace(stringr::str_replace(cid_in, "^", "'"), "$", "'")
    sl <- glue::glue_collapse(sl, sep = ", ")
    sql_where <- glue::glue(sql_where, "AND CAUSABAS IN ({sl})", .sep = " ")
  }
  if (!is.null(more_filters)) {
    sql_where <- glue::glue(sql_where, "AND {more_filters}", .sep = " ")
  }

  sql_query <- glue::glue(sql_select, sql_from, sql_where, sql_group_by, .sep = " ")
  request_body <- list(
    token = list(token = pcdas_token),
    sql = list(sql = list(query = sql_query, fetch_size = fetch_size))
  )
  content <- .brindi_pcdas_query_request(request_body, pcdas_token)

  if (length(content) == 0) {
    content_df <- tibble::tibble(agg = as.numeric(), agg_time = as.character(), freq = as.numeric())
  } else {
    content_df <- .brindi_convert_list_content_to_df(content) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(agg = as.numeric(.data$agg), freq = as.numeric(.data$freq)) %>%
      dplyr::select("agg", "agg_time", "freq")
  }

  if (agg %in% c("regsaude_449_res", "regsaude_449_ocor")) {
    content_df <- dplyr::left_join(
      content_df,
      rpcdas::mun_reg_saude_449,
      by = c(agg = "cod_mun")
    ) %>%
      dplyr::group_by(agg = .data$cod_reg_saude, .data$agg_time) %>%
      dplyr::summarise(freq = sum(.data$freq, na.rm = TRUE), .groups = "drop")
  }

  content_df
}

.brindi_get_sinasc <- function(agg, agg_time = "year", ano, pcdas_token = NULL,
                               sexo = NULL, more_filters = NULL,
                               fetch_size = 65000) {
  if (is.null(pcdas_token)) {
    pcdas_token <- rpcdas::get_pcdas_token_renviron()
  }

  agg_geo <- .brindi_pcdas_agg_geo(agg, "sinasc")
  sql_select <- glue::glue("SELECT {agg_geo} AS agg, COUNT(1) AS freq")
  sql_from <- 'FROM "datasus-sinasc"'
  sql_where <- glue::glue("WHERE ano_nasc IN ({glue::glue_collapse(ano, sep = ', ')})")
  sql_group_by <- glue::glue("GROUP BY {agg_geo}")

  if (agg_time == "year") {
    sql_select <- glue::glue(sql_select, ", DATETIME_FORMAT(data_nasc, 'yyyy') AS agg_time")
    sql_group_by <- glue::glue(sql_group_by, ", agg_time")
  } else if (agg_time == "month") {
    sql_select <- glue::glue(sql_select, ", DATETIME_FORMAT(data_nasc, 'yyyy-MM') AS agg_time")
    sql_group_by <- glue::glue(sql_group_by, ", agg_time")
  } else if (agg_time == "week") {
    sql_select <- glue::glue(sql_select, ", DATETIME_FORMAT(data_nasc, 'yyyy-ww') AS agg_time")
    sql_group_by <- glue::glue(sql_group_by, ", agg_time")
  }

  if (!is.null(sexo)) {
    sql_where <- glue::glue(sql_where, "AND def_sexo = '{sexo}'", .sep = " ")
  }
  if (!is.null(more_filters)) {
    sql_where <- glue::glue(sql_where, "AND {more_filters}", .sep = " ")
  }

  sql_query <- glue::glue(sql_select, sql_from, sql_where, sql_group_by, .sep = " ")
  request_body <- list(
    token = list(token = pcdas_token),
    sql = list(sql = list(query = sql_query, fetch_size = fetch_size))
  )
  content <- .brindi_pcdas_query_request(request_body, pcdas_token)

  if (length(content) == 0) {
    content_df <- tibble::tibble(agg = as.numeric(), agg_time = as.character(), freq = as.numeric())
  } else {
    content_df <- .brindi_convert_list_content_to_df(content) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(agg = as.numeric(.data$agg), freq = as.numeric(.data$freq)) %>%
      dplyr::select("agg", "agg_time", "freq")
  }

  if (agg %in% c("regsaude_449_res", "regsaude_449_ocor")) {
    content_df <- dplyr::left_join(
      content_df,
      rpcdas::mun_reg_saude_449,
      by = c(agg = "cod_mun")
    ) %>%
      dplyr::group_by(agg = .data$cod_reg_saude, .data$agg_time) %>%
      dplyr::summarise(freq = sum(.data$freq, na.rm = TRUE), .groups = "drop")
  }

  content_df
}

.brindi_get_sih <- function(agg, agg_time = "year", ano, pcdas_token = NULL,
                            sexo = NULL, idade_a = NULL, idade_b = NULL,
                            more_filters = NULL, fetch_size = 65000) {
  if (is.null(pcdas_token)) {
    pcdas_token <- rpcdas::get_pcdas_token_renviron()
  }

  agg_geo <- .brindi_pcdas_agg_geo(agg, "sih")
  sql_select <- glue::glue("SELECT {agg_geo} AS agg, COUNT(1) AS freq")
  sql_from <- 'FROM "datasus-sih"'
  sql_where <- glue::glue("WHERE ano_internacao IN ({glue::glue_collapse(ano, sep = ', ')})")
  sql_group_by <- glue::glue("GROUP BY {agg_geo}")

  if (agg_time == "year") {
    sql_select <- glue::glue(sql_select, ", DATETIME_FORMAT(dt_inter, 'yyyy') AS agg_time")
    sql_group_by <- glue::glue(sql_group_by, ", agg_time")
  } else if (agg_time == "month") {
    sql_select <- glue::glue(sql_select, ", DATETIME_FORMAT(dt_inter, 'yyyy-MM') AS agg_time")
    sql_group_by <- glue::glue(sql_group_by, ", agg_time")
  } else if (agg_time == "week") {
    sql_select <- glue::glue(sql_select, ", DATETIME_FORMAT(dt_inter, 'yyyy-ww') AS agg_time")
    sql_group_by <- glue::glue(sql_group_by, ", agg_time")
  }

  if (!is.null(sexo)) {
    sql_where <- glue::glue(sql_where, "AND def_sexo = '{sexo}'", .sep = " ")
  }
  if (!is.null(idade_a) && is.null(idade_b)) {
    sql_where <- glue::glue(sql_where, "AND def_idade_anos <= '{idade_a}'", .sep = " ")
  }
  if (!is.null(idade_b) && is.null(idade_a)) {
    sql_where <- glue::glue(sql_where, "AND def_idade_anos >= '{idade_b}'", .sep = " ")
  }
  if (!is.null(idade_a) && !is.null(idade_b)) {
    sql_where <- glue::glue(
      sql_where,
      "AND def_idade_anos >= '{idade_a}' AND def_idade_anos <= '{idade_b}'",
      .sep = " "
    )
  }
  if (!is.null(more_filters)) {
    sql_where <- glue::glue(sql_where, "AND {more_filters}", .sep = " ")
  }

  sql_query <- glue::glue(sql_select, sql_from, sql_where, sql_group_by, .sep = " ")
  request_body <- list(
    token = list(token = pcdas_token),
    sql = list(sql = list(query = sql_query, fetch_size = fetch_size))
  )
  content <- .brindi_pcdas_query_request(request_body, pcdas_token)

  if (length(content) == 0) {
    content_df <- tibble::tibble(agg = as.numeric(), agg_time = as.character(), freq = as.numeric())
  } else {
    content_df <- .brindi_convert_list_content_to_df(content) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(agg = as.numeric(.data$agg), freq = as.numeric(.data$freq)) %>%
      dplyr::select("agg", "agg_time", "freq")
  }

  if (agg %in% c("regsaude_449_res", "regsaude_449_ocor")) {
    content_df <- dplyr::left_join(
      content_df,
      rpcdas::mun_reg_saude_449,
      by = c(agg = "cod_mun")
    ) %>%
      dplyr::group_by(agg = .data$cod_reg_saude, .data$agg_time) %>%
      dplyr::summarise(freq = sum(.data$freq, na.rm = TRUE), .groups = "drop")
  }

  content_df
}
