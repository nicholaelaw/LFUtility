#' Custom wrapper for write.xlsx
#'
#' Always writes as table with \code{TableStyleMedium16} as table style.
#' For usage, see \link[openxlsx]{write.xlsx}.
#'
#' @export write.xlsx.lf
write.xlsx.lf <- function(x, file, ...) {
  openxlsx::write.xlsx(x, file, asTable = TRUE, tableStyle = xlsxStyles[['tableStyle']], ...)
}
