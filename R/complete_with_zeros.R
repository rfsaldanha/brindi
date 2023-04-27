#' Complete indicator result with zeros
#'
#' Complete indicator result with zeros considering combinations of spatial and temporal aggregation without results.
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param agg_time character. Time aggregation level. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param ano numeric. Year of death.
#' @param save_args logical. Save \code{agg} and \code{agg_time} arguments on results table.
#'
#' @importFrom rlang .data
complete_with_zeros <- function(agg, agg_time, ano, save_args){
  # Spatial aggregation reference table of cods
  if(agg %in% c("mun_res", "mun_ocor")){
    cod_full <- brpop::mun_pop_totals() %>%
      dplyr::select(cod = .data$mun) %>%
      dplyr::distinct()
  } else if(agg %in% c("uf_res", "uf_ocor")){
    cod_full <- brpop::uf_pop_totals() %>%
      dplyr::select(cod = .data$uf) %>%
      dplyr::distinct()
  } else if(agg %in% c("regsaude_res", "regsaude_ocor")){
    cod_full <- brpop::regsaude_pop_totals() %>%
      dplyr::select(cod = .data$regsaude) %>%
      dplyr::distinct()
  }

  # Temporal aggregation reference of dates
  if(agg_time == "year"){
    date_full <- tidyr::tibble(date = as.character(ano))
  } else if(agg_time == "month"){
    date_full <- tidyr::crossing(
      year = ano,
      month = 1:12
    ) %>%
      dplyr::mutate(date = paste0(year, "-", stringr::str_pad(month, 2, pad = 0))) %>%
      dplyr::select(date)
  } else if(agg_time == "week"){
    date_full <- tidyr::crossing(
      year = ano,
      week = 1:53
    ) %>%
      dplyr::mutate(date = paste0(year, "-", stringr::str_pad(week, 2, pad = 0))) %>%
      dplyr::select(date)
  }

  # Complete cod and date by reference tables, filling with zeros
  res <- res %>%
    tidyr::complete(
      cod = cod_full$cod,
      date = date_full$date,
      name = "indi_0001",
      fill = list(value = 0),
      explicit = FALSE
    )

  # Fill original arguments on table if present
  if(save_args == TRUE){
    res <- res %>%
      tidyr::replace_na(list(agg = agg, agg_time = agg_time))
  }

  return(res)
}
