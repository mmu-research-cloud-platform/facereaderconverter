
<!-- README.md is generated from README.Rmd. Please edit that file -->

# facereaderconverter

<!-- badges: start -->

<!-- badges: end -->

The goal of facereaderconverter is to convert FaceReader txt files to
usable csv files that preserve the timings.

## Installation

You can install the development version of facereaderconverter from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sgbstats/facereaderconverter")
```

or

``` r
# install.packages("devtools")
devtools::install_github("sgbstats/facereaderconverter")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(facereaderconverter)
convertFRFiles("testdata/testdata_detailed.txt")
#this will convert that file to a csv in the same name and location and apply defult settings
```

The default settings are values are returned as numeric/time objects and
the names are `janitor`-style `clean_names` so lower case and spaces are
replaced with underscores. When `return_data=FALSE` the metadata from
the conversion is returned and when it is set to `TRUE` then the
function returns the data itself to allow work. If you want to save the
file in another location or under another name then specify the outpath.

``` r
library(facereaderconverter)
convertFRDirectory("testdata")
#this will convert all txt files in the directory to a csv in the same name and location and apply defult settings
```

This function takes similar arguments to files function. The new
arguments are `recrusive` which when true will look for all files in the
whole of the inpath directory. `pattern` which when not null will filter
the files to only those matching the regex pattern, e.g., if you only
wanted to convert the state files.

## Episode coding

### `convert_to_episodes()`

`convert_to_episodes()` detects episodes using hysteresis thresholds, a
delta rule, and a minimum duration filter. It accepts either:

- long data with `id`, `subject`, `video_time`, `emotion`, and `value`,
  or
- wide data with `id`, `subject`, `video_time`, and one column per
  emotion

If `emotion` and `value` are not already present, the function reshapes
wide data to long format internally. If a `frame` column is missing, it
is created from `video_time` using `fps`.

Important arguments:

- `fps`: sampling rate in frames per second
- `T_up`: threshold for entering an episode
- `T_down`: threshold for leaving an episode
- `delta`: minimum change required by the delta rule
- `delta_window`: window size in seconds for the delta rule
- `min_dur_sec`: minimum episode duration in seconds
- `consecutive_missing`: maximum allowed run of missing values while an
  episode is active

The function returns a list with two elements:

- `episodes`: episode-level summaries with `start_frame`, `end_frame`,
  `n_frames`, and `duration_s`
- `coding`: the original coding data annotated with `status` and
  `in_state`

`status` marks detected boundaries:

- `1` for episode start frames
- `0` for episode end frames
- `NA` otherwise

``` r
library(facereaderconverter)

res <- convert_to_episodes(
  coding_df,
  fps = 30L,
  T_up = 0.20,
  T_down = 0.18,
  delta = 0.10,
  delta_window = 0.1,
  min_dur_sec = 0.1,
  consecutive_missing = 150L
)

res$episodes
res$coding
```

Episodes are grouped within each `id`, `subject`, and `emotion`
combination. Episode end frames are adjusted to the last in-state frame
with a non-missing `value`, and episodes shorter than the minimum
duration are removed.

### `add_delta_column()`

`add_delta_column()` appends a `delta` column to a coding data frame
using a windowed delta rule within each `id`, `subject`, and `emotion`
group.

**Delta Rule and Direction:**

- For each window, if the difference between the maximum and minimum
  value (`window_max - window_min`) exceeds the `delta` threshold, a
  delta event is triggered.
- The direction is encoded as:
  - `1` (up): if the maximum occurs after the minimum within the window
    (i.e., an upward change)
  - `0` (down): if the maximum occurs before the minimum within the
    window (i.e., a downward change)
  - `NA`: if no delta event is detected

**Expected columns:**

- `id`
- `subject`
- `emotion`
- `frame` or `video_time`
- `value`

**Key arguments:**

- `delta_window`: window size in seconds used to evaluate change
- `delta`: threshold used for both upward and downward change detection
- `fps`: frames per second used to convert the time window into frames

**Example:**

``` r
library(facereaderconverter)

coding_with_delta <- add_delta_column(
  coding_df,
  delta_window = 0.1,
  delta = 0.1,
  fps = 30L
)
```

The resulting `delta` column will contain `1` for upward events, `0` for
downward events, and `NA` otherwise.
