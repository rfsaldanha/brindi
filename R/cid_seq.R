#' Create CID sequences
#'
#' Helper function to create CID sequences.
#'
#' 'a' letter must be equal to 'b' letter.
#'
#' @param a character. Starting CID code.
#' @param b character. Ending CID code.
#'
cid_seq <- function(a, b){
  a_letter <- stringr::str_extract(string = a, pattern = "[A-Z]")
  a_number <- stringr::str_extract(string = a, pattern = "(\\d)+")

  b_letter <- stringr::str_extract(string = b, pattern = "[A-Z]")
  b_number <- stringr::str_extract(string = b, pattern = "(\\d)+")

  if(a_letter != b_letter){
    stop("'a' letter must be equal to 'b' letter.")
  }

  number_sequence <- stringr::str_pad(
    string = seq(as.numeric(a_number), as.numeric(b_number)),
    width = 2, pad = "0"
  )

  return(paste0(a_letter, number_sequence))
}



