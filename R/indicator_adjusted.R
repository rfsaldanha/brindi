#' Ajusted indicator calculus
#'
#' @param numerador list.
#' @param nome character. PCDaS API token. If not provided, the function will look for it on renvirom.
#' @param agg character. Aggregation acronymin.
#' @param multi integer. Multiplicator for indicator.
#' @param decimals integer. Number of decimals for indicator.
#'
#' @importFrom rlang .data
indicator_adjusted <- function(numerador, year, nome, agg, multi, decimals) {
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

  # Standart population
  mun_pop_age <- brpop::mun_pop_age()

  stdpop <- mun_pop_age %>%
    dplyr::filter(year == 2010) %>%
    dplyr::group_by(age_group) %>%
    dplyr::summarise(stdpop = sum(pop, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(age_group != "Total")

  # Join frequencies and population
  res3 <- dplyr::inner_join(
    res2,
    mun_pop_age %>%
      dplyr::filter(year == year) %>%
      dplyr::mutate(year = as.character(year)),
    by = c("agg" = "code_muni", "agg_time" = "year", "age_group" = "age_group")
  ) %>%
    dplyr::rename(count = freq) %>%
    dplyr::inner_join(stdpop, by = "age_group")

  # Split to list by aggregation
  res4 <- split(res3, res3$agg)

  # Apply age adjust direct mode to each item and convert to data frame
  res5 <- furrr::future_map_dfr(.x = res4, .f = function(x) {
    c(
      id = x$agg[1],
      epitools::ageadjust.direct(
        count = x$count,
        pop = x$pop,
        stdpop = x$stdpop
      )
    )
  })

  return(res5)
}
