// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// elementsAllEqual
bool elementsAllEqual(NumericVector X, double Y);
RcppExport SEXP _LFUtility_elementsAllEqual(SEXP XSEXP, SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type X(XSEXP);
    Rcpp::traits::input_parameter< double >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(elementsAllEqual(X, Y));
    return rcpp_result_gen;
END_RCPP
}
// flipStr
StringVector flipStr(StringVector STR);
RcppExport SEXP _LFUtility_flipStr(SEXP STRSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type STR(STRSEXP);
    rcpp_result_gen = Rcpp::wrap(flipStr(STR));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_LFUtility_elementsAllEqual", (DL_FUNC) &_LFUtility_elementsAllEqual, 2},
    {"_LFUtility_flipStr", (DL_FUNC) &_LFUtility_flipStr, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_LFUtility(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
