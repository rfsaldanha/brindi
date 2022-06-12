#' Convert API content to a tibble
#'
#' Convert content received from a PCDaS API query to a tibble object.
#'
#' @param content Content from API.
#'
#' @return A tibble.
#'
convert_content_to_df <- function(content){
  variables = unlist(content$columns)
  variables = variables[names(variables) == "name"]
  column_names <- unname(variables)
  values = content$rows
  df <- as.data.frame(do.call(rbind,lapply(values,function(r) rbind(unlist(r)))))
  names(df) <- column_names
  return(df)
}
