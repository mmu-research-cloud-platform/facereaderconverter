
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

## File conversion

### `convertFRFiles()`

`convertFRFiles()` reads a single FaceReader `.txt` file, skips the
metadata header, guesses the delimiter, and writes a `.csv` with the
same basename unless `return_data = TRUE`.

``` r
library(facereaderconverter)

convertFRFiles(
  inpath = "testdata/testdata_detailed.txt",
  values_as_numeric = TRUE,
  clean_names = TRUE
)
```

When `values_as_numeric = TRUE`, the `Video Time` column is converted to
`hms` time and detailed-file values are converted to numeric where
possible. When `clean_names = TRUE`, the output names are converted with
`janitor::clean_names()`.

### `convertFRDirectory()`

`convertFRDirectory()` processes all `.txt` files in a directory and
returns metadata invisibly.

``` r
library(facereaderconverter)

convertFRDirectory(
  inpath = "testdata",
  outpath = "junk",
  values_as_numeric = TRUE,
  cores = 2L
)
```

Useful arguments include `recursive` for searching subdirectories,
`pattern` for filtering file names, `metadata_filename` for the output
metadata file, and `duplicate_timecodes_as_error` for handling repeated
time codes.

## Episode coding

### `convert_to_episodes()`

`convert_to_episodes()` detects episodes using hysteresis thresholds, a
delta rule, and a minimum duration filter. It accepts either long data
with `id`, `subject`, `video_time`, `emotion`, and `value`, or wide data
with one column per emotion.

If `emotion` and `value` are not already present, the function reshapes
wide data to long format internally. If `id` or `subject` is missing,
they are added with defaults. If `frame` is missing, it is created from
`video_time` using `fps`.

Important arguments:

- `fps`: sampling rate in frames per second
- `T_up`: threshold for entering an episode
- `T_down`: threshold for leaving an episode
- `delta`: minimum change required by the delta rule
- `delta_window`: window size in seconds for the delta rule
- `min_dur_sec`: minimum episode duration in seconds
- `consecutive_missing`: maximum allowed run of missing values while an
  episode is active

The function returns an `fr_coding` object with `episodes`, `coding`,
and `metadata`.

``` r
library(facereaderconverter)

coding_df <- read.csv("testdata/testdata_detailed.csv") |>
  dplyr::mutate(id = 1, subject = "parent")

coding_df2 <- coding_df |>
  tidyr::pivot_longer(
    cols = c(neutral, happy, sad, angry, surprised, scared, disgusted),
    names_to = "emotion",
    values_to = "value"
  )

res <- convert_to_episodes(
  coding_df2,
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
group. If the input is an `fr_coding` object, the coding data and `fps`
are taken from that object.

``` r
library(facereaderconverter)

coding_df <- read.csv("testdata/testdata_detailed.csv") |>
  dplyr::mutate(id = 1, subject = "parent")

coding_df2 <- coding_df |>
  tidyr::pivot_longer(
    cols = c(neutral, happy, sad, angry, surprised, scared, disgusted),
    names_to = "emotion",
    values_to = "value"
  )

coding_with_delta <- add_delta_column(
  coding_df2,
  delta_window = 0.1,
  delta = 0.1,
  fps = 30L
)
```

The resulting `delta` column contains `1` for upward events, `0` for
downward events, and `NA` otherwise.

### `delta_episodes()`

`delta_episodes()` converts a coding data frame with a `delta` column
into episode summaries.

``` r
library(facereaderconverter)

coding_df <- read.csv("testdata/testdata_detailed.csv") |>
  dplyr::mutate(id = 1, subject = "parent")

coding_df2 <- coding_df |>
  tidyr::pivot_longer(
    cols = c(neutral, happy, sad, angry, surprised, scared, disgusted),
    names_to = "emotion",
    values_to = "value"
  )

episodes <- coding_df2 |>
  add_delta_column(delta = 0.1, delta_window = 0.1, fps = 30L) |>
  delta_episodes(fps = 30L)

episodes
```

The output includes `start_frame`, `end_frame`, `start_time`,
`end_time`, `n_frames`, `duration_s`, `id`, `subject`, `emotion`, and
`run_id`.
