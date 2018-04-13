#include <Rcpp.h>
using namespace Rcpp;

//' Determine if a vector has identical elements
//'
//' @param X Numerical vector
//' @param Y Tolerance for pairwise comparison Default is 1e-6.
//' @export elementsAllEqual
// [[Rcpp::export]]
bool elementsAllEqual(NumericVector X, double Y = 1e-6) {
  for (int i = 0; i < X.size(); ++i) {
    if (X[i] - X[0] > Y || X[0] - X[i] > Y)
      return false;
  }

  return true;
}
