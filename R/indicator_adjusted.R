#' Ajusted indicator computation
#'
#' @param numerador list.
#' @param ano Year of indicator
#' @param nome character. Indicator name.
#' @param pop_source character. Population source, from {brpop} package.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#' @param sex character. Population sex, for {brpop} package.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
indicator_adjusted <- function(
  numerador,
  ano,
  agg,
  agg_time,
  pop_source,
  nome,
  multi,
  decimals,
  sex
) {
  # List to data.frame with age groups
  res1 <- mapply(
    cbind,
    numerador[sapply(numerador, nrow) > 0],
    "age_group" = age_groups_names[sapply(numerador, nrow) > 0],
    SIMPLIFY = FALSE
  ) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  # Fill missing spatial and time aggregations with zero on frequency
  res2 <- complete_with_zeros(res1, agg, agg_time, ano, pop_source) %>%
    dplyr::mutate(
      age_group = dplyr::case_when(
        .data$age_group == "From 80 to 99 years" ~ "From 80 years or more",
        .data$age_group == "From 100 or more" ~ "From 80 years or more",
        TRUE ~ .data$age_group
      )
    ) %>%
    dplyr::group_by(.data$agg, .data$agg_time, .data$age_group) %>%
    dplyr::summarise(freq = sum(.data$freq, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup()

  # Population data by age group
  if (agg %in% c("mun_res", "mun_ocor")) {
    pop_age <- brpop::mun_pop_age(source = pop_source, sex = sex) %>%
      dplyr::rename(agg = .data$code_muni) %>%
      dplyr::mutate(agg = as.numeric(substr(.data$agg, 0, 6)))
  } else if (agg %in% c("uf_res", "uf_ocor")) {
    pop_age <- brpop::uf_pop_age(source = pop_source, sex = sex) %>%
      dplyr::rename(agg = .data$uf) %>%
      dplyr::filter(.data$agg != "5e") %>%
      dplyr::mutate(agg = as.numeric(.data$agg))
  } else if (agg %in% c("regsaude_res", "regsaude_ocor")) {
    pop_age <- brpop::regsaude_pop_age(source = pop_source, sex = sex) %>%
      dplyr::rename(agg = .data$codi_reg_saude) %>%
      dplyr::mutate(agg = as.numeric(.data$agg))
  } else if (agg %in% c("regsaude_449_res", "regsaude_449_ocor")) {
    pop_age <- brpop::regsaude_pop_age(
      type = "reg_saude_449",
      source = pop_source,
      sex = sex
    ) %>%
      dplyr::rename(agg = .data$codi_reg_saude) %>%
      dplyr::mutate(agg = as.numeric(.data$agg))
  }

  # Join frequencies and population by age group, correct age groups for compability
  res3 <- res2 %>%
    dplyr::mutate(year = substr(.data$agg_time, 0, 4)) %>%
    dplyr::inner_join(
      pop_age %>%
        dplyr::filter(.data$year == ano) %>%
        dplyr::filter(.data$age_group != "Total") %>%
        dplyr::mutate(year = as.character(.data$year)),
      by = c("agg" = "agg", "year" = "year", "age_group" = "age_group")
    ) %>%
    dplyr::select(-dplyr::all_of("year")) %>%
    dplyr::rename(count = .data$freq)

  # Format data to tidyrates package
  res4 <- res3 %>%
    dplyr::rename(
      population = .data$pop,
      events = .data$count
    ) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(c("population", "events")))

  # Standard population data
  stdpop <- pop_age %>%
    dplyr::filter(.data$year == 2010) %>%
    dplyr::group_by(.data$age_group) %>%
    dplyr::summarise(population = sum(.data$pop, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$age_group != "Total")

  # Compute standardized rates by direct mode
  res5 <- tidyrates::rate_adj_direct(res4, stdpop, .keys = c("agg", "agg_time"))

  # Round numbers
  res6 <- res5 %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("crude.rate", "adj.rate", "lci", "uci")),
        .fns = ~ round(.x * multi, digits = decimals)
      )
    )

  return(res6)
}
