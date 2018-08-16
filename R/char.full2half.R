#' Convert full-width number characters to half-width numerics
#'
#' @param STR String to be converted
#'
#' @export char.full2half
char.full2half <- function(STR) {
  if (any(Encoding(STR) != "UTF-8")) {
    STR <- iconv(STR, from = "", to = "UTF-8")
  }
  y <- rep('', length(STR))
  for (i in 1L:length(STR)) {
    temp <- paste(STR[i], sep = "", collapse = "")
    y[i] <- unlist(
      strsplit(temp, split = "")
    ) %>% sapply(., function(x) {
      e <- utf8ToInt(x)
      if (e >= 65281 && e <= 65374) {
        return(intToUtf8(e - 65248))
      } else {
        return(x)
      }
    }) %>% paste(., collapse = "")
  }
  return(y)
}
