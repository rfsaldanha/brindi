#' Ajusted indicator computation
#'
#' @param numerador list.
#' @param ano Year of indicator
#' @param nome character. Indicator name.
#' @param pop_source character. Population source, from {brpop} package.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#'
#' @importFrom rlang .data
indicator_adjusted <- function(
  numerador,
  ano,
  agg,
  agg_time,
  pop_source,
  nome,
  multi,
  decimals
) {
  # Passar a usar o tidyrates nessa função, permitindo o uso de várias chaves (agg e agg_time)

  # List to data.frame with age groups
  res1 <- mapply(
    cbind,
    numerador,
    "age_group" = age_groups_names,
    SIMPLIFY = FALSE
  ) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  # Fill missing spatial and time aggregations with zero on frequency
  res2 <- complete_with_zeros(res1, agg, agg_time, ano, pop_source) %>%
    dplyr::mutate(
      age_group = dplyr::case_when(
        age_group == "From 80 to 99 years" ~ "From 80 years or more",
        age_group == "From 100 or more" ~ "From 80 years or more",
        TRUE ~ age_group
      )
    ) %>%
    dplyr::group_by(agg, agg_time, age_group) %>%
    dplyr::summarise(freq = sum(freq, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup()

  # Population data by age group
  if (agg %in% c("mun_res", "mun_ocor")) {
    pop_age <- brpop::mun_pop_age(source = pop_source) %>%
      dplyr::rename(agg = code_muni) %>%
      dplyr::mutate(agg = as.numeric(substr(agg, 0, 6)))
  } else if (agg %in% c("uf_res", "uf_ocor")) {
    pop_age <- brpop::uf_pop_age(source = pop_source) %>%
      dplyr::rename(agg = uf) %>%
      dplyr::mutate(agg = as.numeric(agg))
  } else if (agg %in% c("regsaude_res", "regsaude_ocor")) {
    pop_age <- brpop::regsaude_pop_age(source = pop_source) %>%
      dplyr::rename(agg = codi_reg_saude) %>%
      dplyr::mutate(agg = as.numeric(agg))
  } else if (agg %in% c("regsaude_449_res", "regsaude_449_ocor")) {
    pop_age <- brpop::regsaude_pop_age(
      type = "reg_saude_449",
      source = pop_source
    ) %>%
      dplyr::rename(agg = codi_reg_saude) %>%
      dplyr::mutate(agg = as.numeric(agg))
  }

  # Join frequencies and population by age group, correct age groups for compability
  res3 <- res2 %>%
    dplyr::mutate(year = substr(agg_time, 0, 4)) %>%
    dplyr::inner_join(
      pop_age %>%
        dplyr::filter(year == ano) %>%
        dplyr::filter(age_group != "Total") %>%
        dplyr::mutate(year = as.character(year)),
      by = c("agg" = "agg", "year" = "year", "age_group" = "age_group")
    ) %>%
    dplyr::select(-year) %>%
    dplyr::rename(count = freq)

  # Format data to tidyrates package
  res4 <- res3 %>%
    dplyr::rename(
      population = pop,
      events = count
    ) %>%
    tidyr::pivot_longer(cols = c(population, events))

  # Standard population data
  stdpop <- pop_age %>%
    dplyr::filter(year == 2010) %>%
    dplyr::group_by(age_group) %>%
    dplyr::summarise(population = sum(pop, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(age_group != "Total")

  # Compute standardized rates by direct mode
  res5 <- tidyrates::rate_adj_direct(res4, stdpop, .keys = c("agg", "agg_time"))

  # Round numbers
  res6 <- res5 %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c(crude.rate, adj.rate, lci, uci),
        .fns = ~ round(.x * multi, digits = decimals)
      )
    )

  return(res6)
}
