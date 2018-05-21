#' @export make.yyyymm
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


#' Better \code{as.POSIXct}
#'
#' \code{as.Time} accepts a great variety of formatted dates and time, eg.
#' \code{2016-12-31 12:00:00}, \code{2016-12-31}, \code{20161231-120000},
#' \code{20161231}, etc..
#'
#' @export as.Time
as.Time <- function(STR, FORMAT, OFFSET = '+0800', tz = 'Asia/Chongqing', remove_blank = TRUE, ...) {

  if (any(class(STR) %in% c('POSIXct', 'POSIXt', 'Date'))) {
    return(as.POSIXct(x = STR, tz = tz))
  }

  result <- as.POSIXct(NA)

  if (missing(STR)) STR <- Sys.time()

  if (remove_blank) {
    STR[base::trimws(STR) == ''] <- NA
  }

  # Return NA on all NA
  if (all(is.na(STR))) {
    return(as.POSIXct(STR))
  }

  if (!missing(FORMAT)) {
    result <- as.POSIXct(
      x = STR,
      format = FORMAT,
      tz = tz
    )
  } else {
    pilot  <- STR[!(is.na(STR) | toString(STR) == '')][1L]
    # Check that pilot actually exists
    if (!assertthat::noNA(pilot)) {
      message('as.Time: No non-NA values found.')
    }

    FORMAT <- dttm_pattern[
      lapply(X = pattern, FUN = grepl, x = pilot) == TRUE, format
    ]
    # Check that there is a matching FORMAT
    if (!assertthat::not_empty(FORMAT)) {
      message('as.Time: Could not parse ', pilot, ', no matching format found.')
    }

    result <- as.POSIXct(
      x = paste(STR, OFFSET, sep = ' '),
      format = paste(FORMAT, '%z', sep = ' '),
      tz = tz
    )
  }

  return(result)
}
