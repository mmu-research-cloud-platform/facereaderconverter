#include <Rcpp.h>
#include <deque>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector max_delta_ranges(
    NumericVector value,
    IntegerVector start_row,
    IntegerVector end_row,
    IntegerVector group_start_row,
    int k
) {
  const int n = value.size();
  const int n_ranges = start_row.size();

  if (end_row.size() != n_ranges ||
      group_start_row.size() != n_ranges) {
    stop("Range position vectors must have the same length.");
  }
  if (k < 0) {
    stop("`k` must be non-negative.");
  }

  NumericVector out(n_ranges, NA_REAL);
  if (n == 0 || n_ranges == 0) {
    return out;
  }

  std::deque<int> minimum;
  std::deque<int> maximum;
  int previous_group_start = NA_INTEGER;
  int previous_left = NA_INTEGER;
  int previous_right = NA_INTEGER;

  for (int range_idx = 0; range_idx < n_ranges; ++range_idx) {
    const int start = start_row[range_idx];
    const int end = end_row[range_idx];
    const int group_start = group_start_row[range_idx];

    if (IntegerVector::is_na(start) || IntegerVector::is_na(end) ||
        IntegerVector::is_na(group_start)) {
      continue;
    }
    if (start < 1 || end < start || group_start < 1 ||
        group_start > start || end > n) {
      stop("Range positions must be valid 1-based indices within `value`.");
    }

    const int left = std::max(group_start, start - k) - 1;
    const int right = end - 1;
    const bool new_group = group_start != previous_group_start;

    if (new_group) {
      minimum.clear();
      maximum.clear();
      previous_left = left;
      previous_right = group_start - 2;
      previous_group_start = group_start;
    } else if (left < previous_left || right < previous_right) {
      stop("Ranges must be ordered by non-decreasing positions within groups.");
    }

    for (int value_idx = previous_right + 1; value_idx <= right; ++value_idx) {
      const double current = value[value_idx];
      if (NumericVector::is_na(current)) {
        continue;
      }

      while (!minimum.empty() && value[minimum.back()] >= current) {
        minimum.pop_back();
      }
      minimum.push_back(value_idx);

      while (!maximum.empty() && value[maximum.back()] <= current) {
        maximum.pop_back();
      }
      maximum.push_back(value_idx);
    }

    while (!minimum.empty() && minimum.front() < left) {
      minimum.pop_front();
    }
    while (!maximum.empty() && maximum.front() < left) {
      maximum.pop_front();
    }

    if (!minimum.empty()) {
      out[range_idx] = value[maximum.front()] - value[minimum.front()];
    }

    previous_left = left;
    previous_right = right;
  }

  return out;
}
