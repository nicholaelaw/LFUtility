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


# dttm_pattern <- fread('tmp/dttm_pattern.csv')
# devtools::use_data(dttm_pattern, xlsxStyles, internal = TRUE, overwrite = TRUE)
