## code to prepare `uf_pop` dataset goes here
uf_pop <- mun_pop %>%
  dplyr::mutate(cod = as.numeric(substr(cod, 0, 2))) %>%
  dplyr::group_by(cod, ano) %>%
  dplyr::summarise(pop = sum(pop, na.rm = TRUE)) %>%
  dplyr::ungroup()

usethis::use_data(uf_pop, overwrite = TRUE)
