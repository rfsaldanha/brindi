#' Population denominators
#'
#' This function retrieves populatin denominators.
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param sex character. Defaults to `all`.
#' @param age_group_vec character. Defaults to `all` for all age groups or a vector with specified age groups.
#'
#' @importFrom rlang .data
#' @export
denominator_pop <- function(agg, sex = "all", age_group_vec = "totals"){

  # Sex ALL and age TOTALS
  if(sex == "all" & "totals" %in% age_group_vec){
    if(agg %in% c("mun_res", "mun_ocor")){
      denominador <- brpop::mun_pop_totals() %>%
        dplyr::rename(agg = .data$mun)
    } else if(agg %in% c("uf_res", "uf_ocor")){
      denominador <- brpop::uf_pop_totals() %>%
        dplyr::rename(agg = .data$uf)
    } else if(agg %in% c("regsaude_res", "regsaude_ocor")){
      denominador <- brpop::regsaude_pop_totals() %>%
        dplyr::rename(agg = .data$regsaude)
    }
    # Sex MALE and age TOTALS
  } else if(sex == "male" & "totals" %in% age_group_vec){
    if(agg %in% c("mun_res", "mun_ocor")){
      denominador <- brpop::mun_male_pop_totals() %>%
        dplyr::rename(agg = .data$mun)
    } else if(agg %in% c("uf_res", "uf_ocor")){
      denominador <- brpop::uf_male_pop_totals() %>%
        dplyr::rename(agg = .data$uf)
    } else if(agg %in% c("regsaude_res", "regsaude_ocor")){
      denominador <- brpop::regsaude_male_pop_totals() %>%
        dplyr::rename(agg = .data$regsaude)
    }
    # Sex FEMALE and age TOTALS
  } else if(sex == "female" & "totals" %in% age_group_vec){
    if(agg %in% c("mun_res", "mun_ocor")){
      denominador <- brpop::mun_female_pop_totals() %>%
        dplyr::rename(agg = .data$mun)
    } else if(agg %in% c("uf_res", "uf_ocor")){
      denominador <- brpop::uf_female_pop_totals() %>%
        dplyr::rename(agg = .data$uf)
    } else if(agg %in% c("regsaude_res", "regsaude_ocor")){
      denominador <- brpop::regsaude_female_pop_totals() %>%
        dplyr::rename(agg = .data$regsaude)
    }
    # Sex ALL and age NOT TOTALS
  } else if(sex == "all" & !("totals" %in% age_group_vec)){
    if(agg %in% c("mun_res", "mun_ocor")){
      denominador <- brpop::mun_pop() %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$mun, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$mun)
    } else if(agg %in% c("uf_res", "uf_ocor")){
      denominador <- brpop::uf_pop() %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$uf, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$uf)
    } else if(agg %in% c("regsaude_res", "regsaude_ocor")){
      denominador <- brpop::regsaude_pop() %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$regsaude, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$regsaude)
    }
    # Sex MALE and age NOT TOTALS
  } else if(sex == "male" & !("totals" %in% age_group_vec)){
    if(agg %in% c("mun_res", "mun_ocor")){
      denominador <- brpop::mun_male_pop %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$mun, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$mun)
    } else if(agg %in% c("uf_res", "uf_ocor")){
      denominador <- brpop::uf_male_pop %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$uf, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$uf)
    } else if(agg %in% c("regsaude_res", "regsaude_ocor")){
      denominador <- brpop::regsaude_male_pop() %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$regsaude, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$regsaude)
    }
    # Sex FEMALE and age NOT TOTALS
  } else if(sex == "female" & !("totals" %in% age_group_vec)){
    if(agg %in% c("mun_res", "mun_ocor")){
      denominador <- brpop::mun_female_pop %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$mun, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$mun)
    } else if(agg %in% c("uf_res", "uf_ocor")){
      denominador <- brpop::uf_female_pop %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$uf, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$uf)
    } else if(agg %in% c("regsaude_res", "regsaude_ocor")){
      denominador <- brpop::regsaude_female_pop() %>%
        dplyr::filter(.data$age_group %in% age_group_vec) %>%
        dplyr::group_by(.data$regsaude, .data$year, .data$pop) %>%
        dplyr::summarise(pop = sum(.data$pop, na.rm = TRUE)) %>%
        dplyr::rename(agg = .data$regsaude)
    }
  }


  return(denominador)
}
