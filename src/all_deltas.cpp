#include <Rcpp.h>
#include <deque>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector all_deltas(NumericVector value, int k, double delta) {
  const int n = value.size();
  IntegerVector out(n, NA_INTEGER);

  if (n == 0 || k < 0) {
    return out;
  }

  std::deque<int> max_deque;
  std::deque<int> min_deque;
  int valid_in_window = 0;

  for (int i = 0; i < n; ++i) {
    const int window_start = i - k;

    if (window_start > 0) {
      const double leaving = value[window_start - 1];
      if (!NumericVector::is_na(leaving)) {
        --valid_in_window;
      }
    }

    while (!max_deque.empty() && max_deque.front() < window_start) {
      max_deque.pop_front();
    }
    while (!min_deque.empty() && min_deque.front() < window_start) {
      min_deque.pop_front();
    }

    const double vi = value[i];
    if (!NumericVector::is_na(vi)) {
      ++valid_in_window;

      while (!max_deque.empty() && value[max_deque.back()] < vi) {
        max_deque.pop_back();
      }
      max_deque.push_back(i);

      while (!min_deque.empty() && value[min_deque.back()] > vi) {
        min_deque.pop_back();
      }
      min_deque.push_back(i);
    }

    if (i < k || valid_in_window == 0) {
      continue;
    }

    const int max_pos = max_deque.front();
    const int min_pos = min_deque.front();
    const double d = value[max_pos] - value[min_pos];

    if (d >= delta) {
      out[i] = (max_pos > min_pos) ? 1 : 0;
    }
  }

  return out;
}
