#' @export make.yyyymm
#' @family datetime makers
make.yyyymm <- function(DATE, sep = '-', useChar = FALSE) {
  if (!suppressPackageStartupMessages(require(stringi) & require(lubridate))) {
    stop('This function requires packages stringi and lubridate')
  }

  if (any(class(DATE) == 'character')) {
    thisDate <- as.Time(DATE)
    assertthat::assert_that(!is.na(thisDate), msg = 'Incorrect date string for make.yyyymm.')
  } else if (any(class(DATE) %in% c('POSIXct', 'POSIXt', 'Date'))) {
    thisDate <- DATE
  }

  # 是否使用xxxx年x月？
  if (useChar) {
    result <- paste0(
      lubridate::year(thisDate), '年',
      lubridate::month(thisDate), '月'
    )
  } else {
    result <- paste0(
      year(thisDate),
      sep,
      stringi::stri_pad_left(month(thisDate), width = 2L, pad = '0')
    )
  }

  return(result)
}

#' @export make.yyyymmdd
#' @family datetime makers
make.yyyymmdd <- function(DATE, sep = '-', useChar = FALSE) {

  if (!suppressPackageStartupMessages(require(stringi) & require(lubridate))) {
    stop('This function requires packages stringi and lubridate')
  }

  if (any(class(DATE) == 'character')) {
    thisDate <- as.Time(DATE)
    assertthat::assert_that(!is.na(thisDate), msg = 'Incorrect date string for make.yyyymmdd.')
  } else if (any(class(DATE) %in% c('POSIXct', 'POSIXt', 'Date'))) {
    thisDate <- DATE
  }

  # 是否使用yyyy年m月d日？
  if (useChar) {
    result <- paste0(
      lubridate::year(thisDate), '年',
      lubridate::month(thisDate), '月',
      lubridate::day(thisDate), '日'
    )
  } else {
    result <- paste0(
      year(thisDate),
      sep,
      stringi::stri_pad_left(month(thisDate), width = 2L, pad = '0'),
      sep,
      stringi::stri_pad_left(day(thisDate), width = 2L, pad = '0')
    )
  }

  return(result)
}

#' @export make.mmdd
#' @family datetime makers
make.mmdd <- function(DATE, sep = '-', useChar = FALSE) {

  if (!suppressPackageStartupMessages(require(stringi) & require(lubridate))) {
    stop('This function requires packages stringi and lubridate')
  }

  if (any(class(DATE) == 'character')) {
    thisDate <- as.Time(DATE)
    assertthat::assert_that(!is.na(thisDate), msg = 'Incorrect date string for make.yyyymm.')
  } else if (any(class(DATE) %in% c('POSIXct', 'POSIXt', 'Date'))) {
    thisDate <- DATE
  }

  # 是否使用xxxx年x月？
  if (useChar) {
    result <- paste0(
      lubridate::month(thisDate), '月',
      lubridate::day(thisDate), '日'
    )
  } else {
    result <- paste0(
      stringi::stri_pad_left(month(thisDate), width = 2L, pad = '0'),
      sep,
      stringi::stri_pad_left(day(thisDate), width = 2L, pad = '0')
    )
  }

  return(result)
}

#' @export make.timestamp
#' @family datetime makers
make.timestamp <- function(prefix = '@', useDate = TRUE, useTime = TRUE, tz = 'Asia/Chongqing') {
  result <- prefix
  if (useDate) {
    result <- paste0(
      result,
      format(Sys.time(), format = '%Y%m%d', tz = tz)
    )
  }

  if (useTime) {
    result <- paste0(
      result, 'T',
      format(Sys.time(), format = '%H%M%S', tz = tz)
    )
  }

  return(result)
}

#' A super flexible way to produce endpoints of a time interval.
#'
#' @param STR A character vector or date-time object (\code{date}, \code{POSIXct}, etc.)
#'
#' @param LEN Length of the intended interval
#' @param unit Unit by which to produce the interval. Default is days.
#' @param natural Whether to choose the endpoints as natural boundaries, or use the input as-is. See detail.
#' @param incUB Whether to include the upper boundary (the last second)
#' @return A vector containing the endpoints of the interval, when the input has length 1. A list object the same length of the input if input length is greater than 1.
#'
#' @details
#' When \code{natural} is \code{TRUE}, the function produces the least natural interval containing
#' the input datetime. For example, an input of \code{make.dtRange('2018-08-13', unit = 'months')}
#' would produce \code{"2018-08-01 00:00:00 CST" "2018-08-31 23:59:59 CST"}. Otherwise, the lower bound
#' is set to be the input, with upper bound calculated using \code{LEN} and \code{unit}.
#'
#' @export make.dtRange
#' @family datetime makers
make.dtRange <- function(STR, LEN = 1L, unit = 'days', natural = TRUE, incUB = FALSE) {
  if (length(STR) > 1L) {
    result <- lapply(STR, make.dtRange.singular, LEN = LEN, unit = unit, natural = natural, incUB = incUB)
  } else {
    result <- make.dtRange.singular(STR, LEN, unit, natural, incUB)
  }

  return(result)
}

make.dtRange.singular <- function(STR, LEN = 1L, unit = 'days', natural = TRUE, incUB = FALSE) {

  # months are handled by base method. The rest goes to lubridate
  if (unit == 'months') {
    rangeLen <- base::months(LEN)
  } else {
    rangeLen <- eval(parse(text = paste0('lubridate::', unit)))(LEN)
  }

  if (natural) {
    lb <- lubridate::floor_date(as.Time(STR), unit = unit)
  } else {
    lb <- as.Time(STR)
  }

  if (LEN > 0L) {
    ub <- lb %m+% rangeLen - {if (incUB) 0L else 1L}
  } else if (LEN == 0L) {
    ub <- lb
  } else {
    ub <- lb %m+% rangeLen + {if (incUB) 0L else 1L}
  }

  # return in ascending order, so that it is consistent.
  return(c(min(lb, ub), max(lb, ub)))
}
