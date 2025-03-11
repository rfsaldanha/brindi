#' Ajusted indicator calculus
#'
#' @param numerador list.
#' @param ano Year of indicator
#' @param nome character. Indicator name.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#'
#' @importFrom rlang .data
indicator_adjusted <- function(numerador, ano, nome, multi, decimals) {
  res2 <- mapply(
    cbind,
    numerador,
    "age_group" = age_groups_names,
    SIMPLIFY = FALSE
  ) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble() %>%
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
  mun_pop_age <- brpop::mun_pop_age()

  # Join frequencies and population by age group
  res3 <- dplyr::right_join(
    res2,
    mun_pop_age %>%
      dplyr::filter(year == ano) %>%
      dplyr::filter(age_group != "Total") %>%
      dplyr::mutate(year = as.character(year)),
    by = c("agg" = "code_muni", "agg_time" = "year", "age_group" = "age_group")
  ) %>%
    dplyr::rename(count = freq) %>%
    tidyr::replace_na(list(count = 0))

  # Standard population data
  stdpop <- mun_pop_age %>%
    dplyr::filter(year == 2010) %>%
    dplyr::group_by(age_group) %>%
    dplyr::summarise(stdpop = sum(pop, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(age_group != "Total")

  # Join standard population
  res4 <- res3 %>%
    dplyr::inner_join(stdpop, by = "age_group")

  # Split to list by aggregation
  res5 <- split(res4, res4$agg)

  # Apply age adjust direct mode to each item and convert to data frame
  res6 <- furrr::future_map_dfr(.x = res5, .f = function(x) {
    c(
      id = x$agg[1],
      epitools::ageadjust.direct(
        count = x$count,
        pop = x$pop,
        stdpop = x$stdpop
      )
    )
  })

  # Rename fields and relocate
  res7 <- res6 %>%
    dplyr::mutate(
      name = nome,
      date = ano
    ) %>%
    dplyr::rename(cod = id) |>
    dplyr::relocate(name, cod, date, .before = crude.rate)

  # Round numbers
  res8 <- res7 %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c(crude.rate, adj.rate, lci, uci),
        .fns = ~ round(.x * multi, digits = decimals)
      )
    )

  return(res8)
}
