#include <Rcpp.h>
using namespace Rcpp;

inline bool is_na(double x) { return NumericVector::is_na(x); }

// [[Rcpp::export]]
LogicalVector hysteresis_state(NumericVector v,
                                  int k,
                                  double T_up,
                                  double T_down,
                                  double delta,
                                  int min_len,
                                  int consecutive_missing) {

  int n = v.size();
  LogicalVector state(n, false);

  // k-step delta (NA-safe: dv[i] is NA if v[i] or v[i-k] is NA)
  NumericVector dv(n, NA_REAL);
  if (k >= 1 && n > k) {
    for (int i = k; i < n; ++i) {
      double a = v[i];
      double b = v[i - k];
      if (!is_na(a) && !is_na(b)) dv[i] = a - b;
    }
  }

  bool in_state = false;
  int start_idx = -1;

  int consec_na = 0;
  int last_non_na = -1;   // last index with non-NA v while in_state

  // helper: drop a run if < min_len
  auto enforce_min_len = [&](int s, int e) {
    if (s < 0 || e < s) return;
    int run_len = e - s + 1;
    if (run_len < min_len) {
      for (int j = s; j <= e; ++j) state[j] = false;
    }
  };

  // helper: end an episode at end_idx (inclusive), trimming any TRUE after it
  auto end_episode_at = [&](int end_idx, int i_current) {
    // turn off frames after end_idx up to current index (they may have been set TRUE)
    int from = end_idx + 1;
    if (from < 0) from = 0;
    for (int j = from; j <= i_current && j < n; ++j) state[j] = false;

    // enforce min_len on [start_idx, end_idx]
    enforce_min_len(start_idx, end_idx);

    // reset state machine
    in_state = false;
    start_idx = -1;
    consec_na = 0;
    // last_non_na left as-is (not strictly needed after end)
  };

  for (int i = 0; i < n; ++i) {
    double vi  = v[i];
    bool have_vi = !is_na(vi);

    double dvi = dv[i];
    bool have_dv = !is_na(dvi);

    bool start_evt = (have_vi && vi >= T_up) || (have_dv && dvi >=  delta);
    bool end_evt   = (have_vi && vi <= T_down) || (have_dv && dvi <= -delta);

    if (!in_state) {
      // not currently in an episode
      if (start_evt) {
        in_state = true;
        start_idx = i;
        state[i] = true;

        // initialize NA tracking
        if (have_vi) {
          consec_na = 0;
          last_non_na = i;
        } else {
          consec_na = 1;
          // last_non_na stays whatever it was; but start_evt with NA is unlikely due to NA-safe dv
        }
      } else {
        state[i] = false;
      }
      continue;
    }

    // in_state == true
    // if we have a valid observation, update NA tracking
    if (have_vi) {
      consec_na = 0;
      last_non_na = i;
    } else {
      consec_na += 1;
    }

    // Check explicit end condition first.
    if (end_evt) {
      // End at last observed non-NA prior to i if possible, else at i-1.
      int end_idx = (last_non_na >= start_idx && last_non_na < i) ? last_non_na : (i - 1);

      // Mark current frame i as not in-state
      state[i] = false;

      // Trim any frames after end_idx (e.g., trailing NAs before i)
      if (end_idx >= 0) end_episode_at(end_idx, i - 1);
      else {
        // edge case: end before any valid index
        in_state = false;
        start_idx = -1;
        consec_na = 0;
      }
      continue;
    }

    // No explicit end event -> tentatively keep state TRUE at i
    state[i] = true;

    // Missing-run forced end logic:
    // allow up to consecutive_missing NAs in-state, and terminate only when the run exceeds it
    if (consec_na > consecutive_missing) {
      // Force end at previous non-missing frame (last_non_na)
      int end_idx = last_non_na;

      // If for some reason we never saw a non-NA in this state, drop it all
      if (end_idx < start_idx) {
        for (int j = start_idx; j <= i; ++j) state[j] = false;
        in_state = false;
        start_idx = -1;
        consec_na = 0;
      } else {
        end_episode_at(end_idx, i);
      }
      continue;
    }
  }

  // If we ended while still in_state, trim any trailing NAs and enforce min_len
  if (in_state && start_idx >= 0) {
    int end_idx = (last_non_na >= start_idx) ? last_non_na : (n - 1);

    // Trim after end_idx (if trailing NAs were marked TRUE)
    for (int j = end_idx + 1; j < n; ++j) state[j] = false;

    enforce_min_len(start_idx, end_idx);
  }

  return state;
}