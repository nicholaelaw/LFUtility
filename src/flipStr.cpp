#include <Rcpp.h>
using namespace Rcpp;

//' Flip a string using keys
//'
//' @param STR String vector to be flipped.
// [[Rcpp::export]]
StringVector flipStr(StringVector STR) {
  char key[5] = {'L', 'f', 'R', '=', '@'};
  StringVector result(STR.size());

  for (int i = 0; i < STR.size(); i++) {
    std::string tempRes(STR[i]);
    for (int j = 0; j < STR[i].size(); j++) {
      tempRes[j] = STR[i][j] ^ key[j % (sizeof(key) / sizeof(char))];
    }
    result[i] = tempRes;
  }

  return result;
}
