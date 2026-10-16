#' Population denominators
#'
#' This function retrieves populatin denominators.
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saude of occurence. \code{regsaude_449_res} for regiao de saude (449 units) of residence. \code{regsaude_449_ocor} for regiao de saude (449 units) of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param sex character. Defaults to `all`.
#' @param age_group_vec character. Defaults to `all` for all age groups or a vector with specified age groups.
#' @param pop_source character. Population source, from {brpop} package.
#'
#' @importFrom rlang .data
#' @export
denominator_pop <- function(
  agg,
  sex = "all",
  age_group_vec = "totals",
  pop_source = "datasus"
) {
  # Sex ALL and age TOTALS
  if (sex == "all" & "totals" %in% age_group_vec) {
    if (agg %in% c("mun_res", "mun_ocor")) {
      denominador <- brpop::mun_pop_totals(source = pop_source) %>%
        dplyr::rename(agg = .data$code_muni)
    } else if (agg %in% c("uf_res", "uf_ocor")) {
      denominador <- brpop::uf_pop_totals(source = pop_source) %>%
        dplyr::rename(agg = .data$uf) %>%
        dplyr::filter(agg != "5e")
    } else if (agg %in% c("regsaude_res", "regsaude_ocor")) {
      denominador <- brpop::regsaude_pop_totals(source = pop_source) %>%
        dplyr::rename(agg = .data$codi_reg_saude)
    } else if (agg %in% c("regsaude_449_res", "regsaude_449_ocor")) {
      denominador <- brpop::regsaude_pop_totals(
        type = "reg_saude_449",
        source = pop_source
      ) %>%
        dplyr::rename(agg = .data$codi_reg_saude)
    }
    # Sex MALE and age TOTALS
  } else if (sex == "male" & "totals" %in% age_group_vec) {
    if (agg %in% c("mun_res", "mun_ocor")) {
      denominador <- brpop::mun_male_pop_totals(source = pop_source) %>%
        dplyr::rename(agg = .data$code_muni)
    } else if (agg %in% c("uf_res", "uf_ocor")) {
      denominador <- brpop::uf_male_pop_totals(source = pop_source) %>%
        dplyr::rename(agg = .data$uf) %>%
        dplyr::filter(agg != "5e")
    } else if (agg %in% c("regsaude_res", "regsaude_ocor")) {
      denominador <- brpop::regsaude_male_pop_totals(source = pop_source) %>%
        dplyr::rename(agg = .data$codi_reg_saude)
    } else if (agg %in% c("regsaude_449_res", "regsaude_449_ocor")) {
      denominador <- brpop::regsaude_male_pop_totals(
        type = "reg_saude_449",
        source = pop_source
      ) %>%
        dplyr::rename(agg = .data$codi_reg_saude)
    }
    # Sex FEMALE and age TOTALS
  } else if (sex == "female" & "totals" %in% age_group_vec) {
    if (agg %in% c("mun_res", "mun_ocor")) {
      denominador <- brpop::mun_female_pop_totals(source = pop_source) %>%
        dplyr::rename(agg = .data$code_muni)
    } else if (agg %in% c("uf_res", "uf_ocor")) {
      denominador <- brpop::uf_female_pop_totals(source = pop_source) %>%
        dplyr::rename(agg = .data$uf) %>%
        dplyr::filter(agg != "5e")
    } else if (agg %in% c("regsaude_res", "regsaude_ocor")) {
      denominador <- brpop::regsaude_female_pop_totals(source = pop_source) %>%
        dplyr::rename(agg = .data$codi_reg_saude)
    } else if (agg %in% c("regsaude_449_res", "regsaude_449_ocor")) {
      denominador <- brpop::regsaude_female_pop_totals(
        type = "reg_saude_449",
        source = pop_source
      ) %>%
        dplyr::rename(agg = .data$codi_reg_saude)
    }
    # Sex ALL and age NOT TOTALS
  } else if (sex == "all" & !("totals" %in% age_group_vec)) {
    if (agg %in% c("mun_res", "mun_ocor")) {
      denominador <- brpop::mun_pop_age(source = pop_source) %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$code_muni, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$code_muni)
    } else if (agg %in% c("uf_res", "uf_ocor")) {
      denominador <- brpop::uf_pop_age(source = pop_source) %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$uf, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$uf) %>%
        dplyr::filter(agg != "5e")
    } else if (agg %in% c("regsaude_res", "regsaude_ocor")) {
      denominador <- brpop::regsaude_pop_age(source = pop_source) %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$codi_reg_saude, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$codi_reg_saude)
    } else if (agg %in% c("regsaude_449_res", "regsaude_449_ocor")) {
      denominador <- brpop::regsaude_pop_age(
        type = "reg_saude_449",
        source = pop_source
      ) %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$codi_reg_saude, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$codi_reg_saude)
    }
  }

  denominador <- dplyr::select(denominador, -dplyr::any_of(c("age_group"))) %>%
    dplyr::mutate(agg = as.numeric(substr(.data$agg, 0, 6)))

  return(denominador)
}
