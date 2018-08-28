#' @rdname as.interval
#' @export dmonth
dmonth <- function(STR, ...) {
  x  <- as.Time(STR, ...)
  lb <-
    lubridate::floor_date(x, unit = 'months')
  ub <-
    lubridate::ceiling_date(
      x, unit = 'months', change_on_boundary = TRUE
    ) - 1L

  return(c(lb, ub))
}

#' Make intervals of as.POSIXct
#'
#' @export as.interval
#' @name as.interval
as.interval <- function(FROM, TO, unit = 'days', incUB = FALSE, ...) {
  from <- as.Time(FROM, ...)
  to   <- if (missing(TO)) from else as.Time(TO, ...)

  lb <-
    lubridate::floor_date(from, unit = unit)
  ub <-
    lubridate::ceiling_date(
      to, unit = unit, change_on_boundary = TRUE
    ) - {if (incUB) 0L else 1L}

  return(c(lb, ub))
}
