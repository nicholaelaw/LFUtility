load('./R/sysdata.rda')

xlsxStyles <- list(
  basic   = openxlsx::createStyle(fontName = 'Noto Sans Mono CJK SC Regular', fontSize = 11),
  count   = openxlsx::createStyle(numFmt   = '_ * #,##0 "笔"_ ;_ * -#,##0 "笔"_ ;_ * "-"_ ;_ @_ '),
  times   = openxlsx::createStyle(numFmt   = '_ "第" #0 "期"_ ;_ * "-"_ ;_ @_ '),
  money   = openxlsx::createStyle(numFmt   = '_ * #,##0.00 "元"_ ;_ * -#,##0.00 "元"_ ;_ * "-"_ ;_ @_ '),
  moneyX  = openxlsx::createStyle(numFmt   = '_ * #,##0 "万元"_ ;_ * -#,##0 "万元"_ ;_ * "-"_ ;_ @_ '),
  pc      = openxlsx::createStyle(numFmt   = '_ * 0.00%_ ;_ * -0.00%_ ;_ * "-"??_ ;_ @_ '),
  pc_int  = openxlsx::createStyle(numFmt   = '_ * 0%_ ;_ * -0%_ ;_ * "-"??_ ;_ @_ '),
  headr   = openxlsx::createStyle(halign   = 'center', valign = 'center', wrapText = TRUE),
  norm    = openxlsx::createStyle(halign   = 'left'),
  month   = openxlsx::createStyle(numFmt   = 'yyyy"年"m"月"; @ '),
  mday    = openxlsx::createStyle(numFmt   = 'm"月"d"日"; @ '),
  sep     = openxlsx::createStyle(border   = 'Bottom', borderStyle = 'double'),
  comment = openxlsx::createStyle(fontName = 'Noto Sans CJK SC Light', fontSize = 9),
  tableStyle = 'TableStyleMedium16'
)

xlsxStyles[['sep']]$borderBottomColour <- list(theme='1" tint="0.6')
xlsxStyles[['comment']]$fontColour     <- list(theme='2" tint="0.8')

devtools::use_data(dttm_pattern, xlsxStyles, internal = TRUE, overwrite = TRUE)
