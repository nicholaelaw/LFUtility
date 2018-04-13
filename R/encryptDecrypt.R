#' Scramble/Unscramble Strings
#'
#' Intended to protect sensitive information from human eyes (but not from serious attacks).
#'
#' @param STR Character vector to be encrypted/decrypted.
#' @param base64 Whether or not to use base64 encoding/decoding. Default is TRUE.
#'
#' @export encrypt
encrypt <- function(STR, base64 = TRUE) {
  result <-
    enc2utf8(STR) %>%
    flipStr() %>%
    serialize(connection = NULL, version = 2)

  if (base64) {
    return(jsonlite::base64_enc(result))
  } else {
    return(result)
  }
}

#' @rdname encrypt
#' @export decrypt
decrypt <- function(STR, base64 = TRUE) {
  if (base64) {
    temp <- jsonlite::base64_dec(STR)
  } else {
    temp <- STR
  }

  return(
    temp %>% unserialize() %>% flipStr() %>% `Encoding<-`('UTF-8')
  )
}
