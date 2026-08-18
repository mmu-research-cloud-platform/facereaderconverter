#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector all_deltas(NumericVector value, int k, double delta) {
  int n = value.size();
  IntegerVector out(n, NA_INTEGER);

  for (int i = k; i < n; ++i) {
    double window_max = NA_REAL;
    double window_min = NA_REAL;
    int max_pos = -1;
    int min_pos = -1;
    bool any_valid = false;

    for (int j = i - k; j <= i; ++j) {
      double vj = value[j];
      if (NumericVector::is_na(vj)) {
        continue;
      }

      if (!any_valid) {
        window_max = window_min = vj;
        max_pos = min_pos = j;
        any_valid = true;
      } else {
        if (vj > window_max) {
          window_max = vj;
          max_pos = j;
        }
        if (vj < window_min) {
          window_min = vj;
          min_pos = j;
        }
      }
    }

    if (!any_valid) {
      continue;
    }

    double d = window_max - window_min;
    if (d >= delta) {
      out[i] = (max_pos > min_pos) ? 1 : 0;
    }
  }

  return out;
}
