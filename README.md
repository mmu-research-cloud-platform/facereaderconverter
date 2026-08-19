
<!-- README.md is generated from README.Rmd. Please edit that file -->

# facereaderconverter

<!-- badges: start -->

<!-- badges: end -->

The package is a series of functions for converting FaceReader output
files into a more analysis-friendly format, and for detecting episodes
of emotion from the time series data. It also includes some utilities
for downstream analysis.

Tested to work with FaceReader 9.1.

## Installation

You can install the development version of facereaderconverter from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mmu-research-cloud-platform/facereaderconverter")
```

or

``` r
# install.packages("devtools")
devtools::install_github("mmu-research-cloud-platform/facereaderconverter")
```

## File conversion

### `loadFRfile()`

`loadFRfile()` loads a FaceReader export into memory without writing an
output file. It dispatches to the TXT, Excel, or CSV reader based on the
file extension.

``` r
library(facereaderconverter)

loadFRfile(
  inpath = "testdata/testdata_detailed.txt",
  values_as_numeric = TRUE,
  clean_names = TRUE
)
```

Use this when you want a parsed data frame directly from a FaceReader
export.

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

### `convertFRExcelFiles()`

`convertFRExcelFiles()` reads a single FaceReader `.xlsx` file, detects
the header row automatically, and returns the parsed data unless
`return_data = FALSE`.

``` r
library(facereaderconverter)

convertFRExcelFiles(
  inpath = "FaceReaderOutput.xlsx",
  return_data = TRUE,
  values_as_numeric = TRUE,
  clean_names = TRUE
)
```

When `return_data = FALSE`, the function returns metadata about the
workbook import.

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
time codes. The metadata output includes success and failure status
columns.

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

## Utilities and downstream analysis

### `to_seconds()`

`to_seconds()` converts FaceReader-style timestamps such as `hh:mm:ss`
or `hh:mm:ss.mmm` into numeric seconds.

``` r
library(facereaderconverter)

to_seconds(c("00:00:10", "00:00:10.500", "00:01:00"))
#> [1] 10.0 10.5 60.0
```

Use this when you need a numeric time variable for plotting or joining.

### `parse_time_to_frame()`

`parse_time_to_frame()` converts timestamps to frame indices at a given
sampling rate. It accepts `HH:MM:SS`, `MM:SS`, or plain seconds.

``` r
library(facereaderconverter)

parse_time_to_frame(c("00:00:10.5", "1:23.5", "83.5"), fps = 30)
#> [1]  315 2505 2505
```

This is the helper used internally when `video_time` is present but
`frame` is not.

### `map_paths()`

`map_paths()` remaps files from one root directory to another while
preserving the relative folder structure.

``` r
library(facereaderconverter)

map_paths(
  input_dir = "data/raw",
  output_dir = "data/converted",
  files = c(
    "data/raw/session1/a.txt",
    "data/raw/session2/b.txt"
  )
)
```

This is useful when converting a directory of files into a parallel
output tree.

### `locf()`

`locf()` applies last-observation-carried-forward logic to an
`fr_coding` object and returns the imputed coding table plus episode
summaries.

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

fr <- convert_to_episodes(coding_df2, fps = 30L)
locf(fr)
```

Use this when short missing stretches should inherit the most recent
non-missing run assignment.

### `synchrony()`

`synchrony()` compares the episode coding for one subject against
another subject within each `id` and emotion.

``` r
library(facereaderconverter)

coding <- data.table::data.table(
  id = rep(1L, 6),
  subject = rep(c("teen", "parent"), each = 3),
  emotion = "happy",
  video_time = rep(1:3, 2),
  value = c(0.1, 0.2, 0.3, 0.1, 0.2, 0.3),
  in_state = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE),
  run_id = c(1L, 1L, 1L, 2L, 2L, 2L)
)

episodes <- data.table::data.table(
  id = 1L,
  subject = c("teen", "parent"),
  emotion = "happy",
  run_id = c(1L, 2L),
  start_frame = c(2L, 3L),
  end_frame = c(2L, 3L)
)

coded_data <- structure(
  list(coding = coding, episodes = episodes),
  class = c("fr_coding", "list")
)

synchrony(coded_data, subject_names = c("teen", "parent"), missing_threshold = 1)
```

The result reports denominator and numerator subjects, the emotion, the
number of episodes, and synchrony.

### `reaction_rate()`

`reaction_rate()` estimates the proportion of eligible episodes that
contain at least one delta reaction after an initial exclusion window.

``` r
library(facereaderconverter)

coded_data <- data.table::data.table(
  id = rep(1L, 8),
  subject = rep(c("teen", "parent"), each = 4),
  emotion = "happy",
  frame = rep(1:4, 2),
  delta = c(1L, 1L, 0L, 1L, 0L, 1L, 1L, 0L)
)

reaction_rate(coded_data, fps = 30)
```

This is useful when you want a simple summary of reaction frequency by
subject and emotion.
