
# format2pretty -----------------------------------------------------------

#' Various formatting wrappers to make numbers pretty
#'
#' These functions wraps methods from \code{formattable} package to
#' transform appearance of numeric variables.
#'
#' @param X A vector, most likely numeric, to be formatted.
#' @param digits Number of decimal digits to be shown.
#' @param with_unit whether or not to attach a unit after the number.
#'
#' @return The value of the input is unchanged, but its \code{print} method
#' is modified to produce the desired output.
#'
#' @seealso \code{\link[formattable]{formattable}}
#'
#' @name format2pretty
#' @family Pretty formatting functions
#'
#' @author Mu Yifeng
NULL


#' @rdname format2pretty
#' @export format2count
format2count <- function(X, with_unit = TRUE) {
  result <- formattable::formattable(
    x = X, format = 'd', big.mark = ',', digits = 0L,
    postproc = ifelse(with_unit, 'postproc2count', 'postprocNULL')
  )
  return(result)
}

#' @rdname format2pretty
#' @export format2percent
format2percent <- function(X, digits = 0L) {
  result <- formattable::percent(x = X, digits = digits, format = 'f')
  return(result)
}

#' @rdname format2pretty
#' @export format2times
format2times  <- function(X) {
  result <- formattable::formattable(
    x = X, format = 'd', big.mark = ',', digits = 0L,
    postproc = 'postproc2times'
  )
  return(result)
}

#' @rdname format2pretty
#' @export format2money
format2money <- function(X, digits = 0L, scale = 10000L, with_unit = TRUE) {
  result <- formattable::formattable(
    x = X / scale, format = 'f', big.mark = ',', digits = digits,
    postproc = ifelse(with_unit, 'postproc2money', 'postprocNULL')
  )
  return(result)
}

#' @rdname format2pretty
#' @export f2money
f2money  <- function(X, digits = 2L, scale = 1, prefix = '', suffix = '') {
  result <- formattable::formattable(
    x = X / scale, format = 'f', big.mark = ',', digits = digits
  )
  return(result %>% formattable::prefix(prefix = '') %>% formattable::suffix(suffix = ''))
}

#' @rdname format2pretty
#' @export format2Ym
format2Ym  <- function(X) {
  result <- formattable::formattable(
    x = X, format = '%Y年%m月', formatter = 'format.Date'
  )
  return(result)
}

#' @rdname format2pretty
#' @export format2Ymd
format2Ymd  <- function(X) {
  result <- formattable::formattable(
    x = X, format = '%Y年%m月%d日', formatter = 'format.Date'
  )
  return(result)
}

#' @rdname format2pretty
#' @export format2days
format2days <- function(X, digits = 0L) {
  result <-
    formattable::formattable(
      x = X, format = 'f', big.mark = ',', digits = digits,
      preproc = 'preproc2days', postproc = 'postproc2days'
    )
  return(result)
}


# Preprocessing -----------------------------------------------------------
#' @export preproc2money
preproc2money  <- function(VALUE) {
  # result <- VALUE / 10000
  return(result)
}

#' @export preproc2days
preproc2days <- function(VALUE) {
  if (class(VALUE) == 'difftime') {
    result <- as.numeric(VALUE, units = 'days')
  } else {
    result <- as.numeric(VALUE)
  }
  return(result)
}


# Postprocessing ----------------------------------------------------------
#' @export postproc2money
postproc2money <- function(STR, VALUE) {
  result <- ifelse(VALUE == 0 | is.na(VALUE), '--', paste0(STR, '万元'))
  return(result)
}
#' @export postproc2times
postproc2times <- function(STR, VALUE) {
  result <- ifelse(VALUE <= 0 | is.na(VALUE), '--', paste0('第', STR, '期'))
  return(result)
}
#' @export postproc2count
postproc2count <- function(STR, VALUE) {
  result <- ifelse(VALUE == 0 | is.na(VALUE), '--', paste0(STR, '笔'))
  return(result)
}
#' @export postprocNULL
postprocNULL <- function(STR, VALUE) {
  return(STR)
}
#' @export postproc2days
postproc2days <- function(STR, VALUE) {
  result <- paste0(STR, '天')
  return(result)
}
