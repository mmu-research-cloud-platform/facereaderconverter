
<!-- README.md is generated from README.Rmd. Please edit that file -->

# facereaderconverter

<!-- badges: start -->

<!-- badges: end -->

The package is a series of functions for converting FaceReader output
files into a more analysis-friendly format, and for detecting episodes
of emotion from the time series data. It also includes some utilities
for downstream analysis.

Tested to work with FaceReader 9.1 and 10

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

**Arguments:** `inpath` is the path to a `.txt`, `.xlsx`, or `.csv`
export. `values_as_numeric` controls conversion of video times and
detailed FaceReader values to numeric-friendly types. `clean_names`
applies `janitor::clean_names()` to column names. `fail_codes` adds
`fail_code` for detailed exports (`0` for success, `1` for `FIT_FAILED`,
and `2` for `FIND_FAILED`). `duplicate_timecodes_as_error` controls
whether duplicate time codes stop processing (`TRUE`) or produce a
warning (`FALSE`). `sheet` selects the Excel sheet when `inpath` is an
`.xlsx` file. Additional arguments in `...` are passed to the underlying
reader; for CSV files they are passed to `readr::read_csv()`.

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

**Arguments:** `inpath` is the path to one existing `.txt` file.
`outpath` determines the output location and basename; it defaults to
the input path with a `.csv` extension. `return_data` returns the parsed
data invisibly instead of writing a CSV and returning import metadata.
`values_as_numeric`, `clean_names`, `fail_codes`, and
`duplicate_timecodes_as_error` have the same meanings as in
`loadFRfile()`. Additional arguments in `...` are passed to
`janitor::clean_names()` when names are cleaned.

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

**Arguments:** `inpath` is the path to one existing `.xlsx` file.
`return_data` controls whether the parsed data (`TRUE`) or import
metadata (`FALSE`) is returned invisibly. `values_as_numeric`,
`clean_names`, `fail_codes`, and `duplicate_timecodes_as_error` have the
same meanings as for the text importer. `sheet` is a positive sheet
number, starting at `1`; the default is the first sheet. Additional
arguments in `...` are passed to `janitor::clean_names()` when names are
cleaned.

### `convertFRDirectory()`

`convertFRDirectory()` processes supported `.txt`, `.xlsx`, and `.csv`
FaceReader exports in a directory, writes converted CSV files, and
returns conversion metadata invisibly.

``` r
library(facereaderconverter)

convertFRDirectory(
  inpath = "testdata",
  outpath = "junk",
  values_as_numeric = TRUE,
  cores = 2L
)
```

**Arguments:** `inpath` is the source directory. `outpath` is the
destination directory and defaults to `inpath`; when different, the
input folder structure is reproduced below it. `recursive` controls
whether nested directories are searched. `pattern` optionally restricts
input **basenames** using a case-sensitive regular expression; it is
applied in addition to the supported-extension filter. `NULL` processes
all supported `.txt`, `.xlsx`, and `.csv` inputs. `values_as_numeric`,
`clean_names`, `fail_codes`, and `duplicate_timecodes_as_error` are
passed to the file readers. `save_metadata` is the directory in which
the metadata CSV is saved, or `NULL` to skip saving it.
`metadata_filename` sets that CSV’s filename. `cores` controls parallel
workers; `0` automatically selects a worker count. Additional arguments
in `...` are passed to the relevant file importer.

For example, use `pattern = "detailed\\.xlsx$"` for detailed Excel
exports, `pattern = "_state\\.txt$"` for state text exports, or
`pattern = "8895.*(detailed|state)\\.xlsx$"` to select a participant’s
detailed and state workbooks. Generated CSVs and metadata CSVs are
excluded from subsequent input discovery.

The returned metadata contains `inpath`, `outpath`, `video_filename`,
`time`, `type`, `status` (`"Success"` or `"Fail"`), and `error`. Files
that fail are recorded rather than stopping the whole directory
conversion. By default this metadata is also written as `metadata.csv`
in `outpath`; set `save_metadata = NULL` to omit it.

## Synchrony moments pipeline

`synchrony_moments_pipeline()` provides an end-to-end workflow: it
recursively finds videos and detailed FaceReader `.txt` or `.xlsx`
exports, detects emotion episodes for the two participants in each
video, identifies shared synchronous episodes, and exports clips. By
default it selects the ten highest-ranked shared `happy` intervals per
video and creates a separate ZIP archive for each video.

FaceReader exports are normally matched to videos by the basename in
their `Filename` metadata, ignoring directory components, case, and file
extensions. Each matched video must have exactly two detailed exports
whose `Participant Name` columns each contain one distinct non-missing
value. State outputs are ignored and CSV exports are not accepted
because they do not retain the source-video header metadata required for
validation. FFmpeg and FFprobe must be available on `PATH` to create
clips.

### Interactive R

The compact example below uses common defaults.

``` r
library(facereaderconverter)

result <- synchrony_moments_pipeline(
  inpath = "data/study",
  output_dir = "data/synchrony-clips",
  fps = 30L,
  n = 10L,
  emotion = "happy"
)

result$manifest
result$videos
result$results
```

### Full configuration

For full control, this example groups every pipeline argument by stage;
remove settings whose defaults are appropriate.

``` r
result <- synchrony_moments_pipeline(
  # Discovery and pairing
  inpath = "data/study",
  output_dir = "data/synchrony-clips",
  video_pattern = "\\.(mp4|mov|mkv|avi)$",
  video_path = NULL,
  subject_from_filename = FALSE,

  # Episode detection
  T_up = 0.20,
  T_down = 0.10,
  delta = 0.10,
  delta_window = 0.2,
  min_dur_sec = 0.1,
  consecutive_missing = 150L,
  fps = 30L,
  cores = 0L,

  # Shared synchrony detection
  time_limit = 3,
  time_limit_frames = NULL,
  constraint_method = "episode",
  missing_threshold = 0,
  exclude_emotions = "neutral",

  # Clip selection and export
  n = 10L,
  emotion = "happy",
  optimised_subject = "both",
  only_synchronies = TRUE,
  buffer = c(before = 2, after = 1),
  buffer_units = "seconds",
  output = "zip",
  overwrite = FALSE,
  ffmpeg = "ffmpeg"
)
```

For a successful run, `result$manifest` records every discovered
FaceReader export and its validated association. Invalid or ambiguous
associations stop the pipeline with an error. `result$videos` records
each validated video and its two paired exports. `result$results` is
named by pipeline video ID; each entry contains the combined
`coding_input`, the `coded_data` `fr_coding` object, the
`shared_moments` table, the returned `clip_manifest`, and the clip
archive path.

Some datasets, including the Brazil fixture used in the clip-export
tests, contain two single-participant FaceReader exports but one
side-by-side source video. Those exports may also lack
`Participant Name`. Use `video_path` to deliberately pair all detailed
exports with that video, and `subject_from_filename = TRUE` to use each
export basename as its participant label:

``` r

brazil_dir <- file.path(Sys.getenv("TEST_DATA"), "brazil")

brazil_result <- synchrony_moments_pipeline(

  inpath = brazil_dir,

  video_path = file.path(brazil_dir, "ID100024_side_by_side.mp4"),

  subject_from_filename = TRUE,

  output_dir = "brazil-synchrony-clips",

  n = 1L,

  overwrite = TRUE,

  cores = 1L

)
```

### Command line

Install the package before using the command-line interface. From a
local checkout, run:

``` r

remotes::install_local(".")
```

The CLI script is installed with the package rather than registered
automatically on your shell `PATH`. Run it from R to obtain its
installed location, then pass that path to `Rscript`:

``` r

cli <- system.file("scripts", "synchrony-moments", package = "facereaderconverter")

cli
```

``` sh

Rscript "PATH_PRINTED_ABOVE" \

  --input data/study \

  --output-dir data/synchrony-clips \

  --fps 30 \

  --n 10 \

  --emotion happy
```

To make `synchrony-moments` available as a regular shell command, copy
or symlink the installed script to a directory on `PATH`. On Unix-like
systems:

``` sh

ln -s "$(Rscript -e 'cat(system.file("scripts", "synchrony-moments", package = "facereaderconverter"))')" ~/.local/bin/synchrony-moments

chmod +x ~/.local/bin/synchrony-moments
```

After that one-time setup, supply a root directory with `--input`; use
`--output-dir` to keep generated clips separate from source files:

``` sh

synchrony-moments \

  --input data/study \

  --output-dir data/synchrony-clips \

  --fps 30 \

  --n 10 \

  --emotion happy
```

For Brazil-style side-by-side recordings, explicitly supply the source
video and use export filenames as participant labels:

``` sh

synchrony-moments \

  --input "$TEST_DATA/brazil" \

  --video "$TEST_DATA/brazil/ID100024_side_by_side.mp4" \

  --subject-from-filename \

  --output-dir brazil-synchrony-clips \

  --n 1 \

  --overwrite
```

CLI option names use lowercase kebab case (`--time-limit-frames` for
`time_limit_frames`). Comma-separate multiple emotions, for example
`--emotion happy,surprised`, and use `--buffer 2,1` for asymmetric
`before,after` buffering. Set logical selection explicitly with
`--only-synchronies TRUE` or use `--no-only-synchronies` for individual
episodes. Run `synchrony-moments --help` for the complete option
reference. Each successful CLI run writes
`synchrony-moments-manifest.csv` at the output root and creates one clip
ZIP archive for each video with selected intervals.

## Episode coding

### `convert_to_episodes()`

`convert_to_episodes()` converts FaceReader coding data into detected
episodes using hysteresis thresholds, a delta-based reaction signal, and
a minimum duration filter. It accepts long data with `id`, `subject`,
and either `video_time` or `frame`, plus `emotion` and `value`; if
`emotion` and `value` are missing, wide data are reshaped to long format
internally.

The function requires `id` and `subject`. If `frame` is not supplied, it
is derived from `video_time` using `fps`. `T_up` and `T_down` control
episode entry and exit, while `delta` and `delta_window` define the
returned delta-up signal rather than episode boundaries.

Important arguments:

- `fps`: sampling rate in frames per second
- `T_up`: threshold for entering an episode
- `T_down`: threshold for leaving an episode
- `delta`: minimum change required by the delta rule
- `delta_window`: window size in seconds for the delta rule
- `min_dur_sec`: minimum episode duration in seconds
- `consecutive_missing`: maximum allowed run of missing values while an
  episode is active
- `cores`: number of threads used by `data.table`

The function returns an `fr_coding` object with four components:
`episodes`, `deltas`, `coding`, and `metadata`. `episodes` contains the
detected episode spans, `deltas` contains delta-up reaction events, and
`coding` contains the annotated frame-level data with episode and delta
markers.

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
res$deltas
res$coding
```

Episodes are grouped within each `id`, `subject`, and `emotion`
combination. Episode end frames are adjusted to the last in-state frame
with a non-missing `value`, and episodes shorter than the minimum
duration are removed. Delta rows are tracked separately for downstream
functions such as `reaction_rate()`.

**Arguments:** `coding_df` is a data frame containing `id`, `subject`,
and either `video_time` or `frame`, plus either long-format `emotion`
and `value` columns or wide emotion columns. `T_up` and `T_down` are
probabilities in `[0, 1]`; `T_up` must be at least `T_down`. `delta` is
the positive minimum change used to flag delta-up rows. `delta_window`
is the positive time window in seconds used to determine the lag for
that change. `min_dur_sec` removes episodes shorter than this duration.
`consecutive_missing` is the maximum number of consecutive missing
frames tolerated while an episode is active. `fps` is the positive
integer sampling rate used to convert times and durations. `cores` sets
the `data.table` thread count; `0` uses its automatic setting.

## Delta helpers

### `add_delta_column()`

`add_delta_column()` adds or replaces a frame-level `delta` column
containing the delta-up signal. It is deprecated because
`convert_to_episodes()` now calculates and returns the same signal as
part of its `fr_coding` result; use `convert_to_episodes()` for new
workflows.

**Arguments:** `coding` is a raw data frame or an `fr_coding` object. A
raw table should contain `emotion` and `value` in long format, or wide
emotion columns, plus `frame` or `video_time`; missing `id` and
`subject` columns are filled with defaults. `delta` is the positive
minimum change required for a delta-up event. `delta_window` is the time
window in seconds used to calculate the lag between observations. `fps`
is the positive integer sampling rate used to convert `video_time` to
frames. `cores` controls the `data.table` thread count; `0` uses its
automatic setting.

The function returns a `data.table`. When an `fr_coding` object is
supplied, its `coding` component is used and its stored `fps` is
applied.

### `delta()`

`delta()` is a deprecated alias for `add_delta_column()` and accepts the
same arguments and returns the same result. Use `convert_to_episodes()`
instead of either legacy helper.

**Arguments:** `coding`, `delta`, `delta_window`, `fps`, and `cores`
have the same meanings as in `add_delta_column()`.

### `delta_episodes()`

`delta_episodes()` groups consecutive rows where `delta == 1` into
delta-event episodes. It is intended for a coding table containing `id`,
`subject`, `emotion`, `frame`, `video_time`, `value`, and `delta`
columns, and returns a `data.table` of event spans. Episode endpoints
are adjusted to the last frame with a non-missing `value` when possible.

**Arguments:** `coding` is a coding table with a precomputed `delta`
column; unlike `add_delta_column()`, this function does not calculate
the delta signal. `fps` is the positive sampling rate used to calculate
durations. `cores` controls the `data.table` thread count; `0` uses its
automatic setting.

## Downstream analysis

### `synchrony()`

`synchrony()` compares the episode coding for one subject against
another subject within each `id` and emotion. The `missing_threshold`
argument controls how much missing comparison data is tolerated within a
denominator episode before that episode is dropped.

``` r
library(facereaderconverter)

synchrony(coded_data, missing_threshold = 0.5)
```

The result reports denominator and numerator subjects, the emotion, the
number of episodes, and synchrony. The `id` column keeps the same type
as the input data.

**Arguments:** `coded_data` is the `fr_coding` object returned by
`convert_to_episodes()`. `subject` and `id` name the subject and
case/dyad columns in both `coded_data$coding` and `coded_data$episodes`.
`missing_threshold` is a proportion from `0` to `1`; an episode is
retained only when the comparison subject is present for at least that
proportion of its frames. `exclude_emotions` lists emotions omitted from
denominator episodes and defaults to `"neutral"`.

### `synchrony_by_episode()`

`synchrony_by_episode()` returns the episode-level comparison table used
by `synchrony()`. It includes one row per denominator episode, the
numerator subject matched to that episode, and a logical `synchrony`
flag indicating whether the comparison subject was present during the
denominator episode.

``` r
library(facereaderconverter)

synchrony_by_episode(coded_data, missing_threshold = 0.5)
```

Use this when you need the underlying episode-level matches rather than
the aggregated synchrony summary.

**Arguments:** `coded_data`, `subject`, `id`, `missing_threshold`, and
`exclude_emotions` have the same meanings as in `synchrony()`. This
function returns the unaggregated comparison rows, including the matched
subjects, `run_id`, presence proportion, and logical synchrony result.

### `shared_synchronous_episodes()`

`shared_synchronous_episodes()` identifies overlapping, same-emotion
episode pairs that are synchronous. Each unordered subject pair is
returned once for each overlapping pair of source episodes. The result
includes the inclusive shared frame interval, source episode
identifiers, each subject’s maximum emotion value, the larger maximum,
and their combined value.

``` r
library(facereaderconverter)

shared <- shared_synchronous_episodes(
  coded_data,
  exclude_emotions = "neutral"
)

head(shared)
```

This function is useful when synchrony needs to be linked back to
specific episode intervals, for example when selecting video clips.
`coded_data` must be the `fr_coding` object returned by
`convert_to_episodes()` and its episode table must contain `max_value`.
`subject` and `id` select the subject and case/dyad columns.
`time_limit`, `time_limit_frames`, and `constraint_method` control the
comparison window in the same way as `synchrony()`. `fps` converts a
time limit to frames, `missing_threshold` controls the required
comparison-subject presence, and `exclude_emotions` removes emotions
before matching.

The returned table contains `id`, `emotion`, `subject1`, `subject2`,
`subject1_run_id`, `subject2_run_id`, `start_frame`, `end_frame`,
`subject1_max_value`, `subject2_max_value`, `max_value`, and
`combined_value`. Rows are ordered by case, emotion, subject pair, and
source episode identifiers.

### `export_shared_synchrony_clips()`

`export_shared_synchrony_clips()` ranks shared synchronous intervals by
`combined_value` and uses FFmpeg to export the corresponding video
segments. It returns a clip manifest invisibly and, for ZIP output,
includes the manifest as `manifest.csv` in the archive.

``` r
library(facereaderconverter)

shared <- shared_synchronous_episodes(coded_data)

export_shared_synchrony_clips(
  coded_data,
  shared,
  video_paths = c("1" = "recording.mp4"),
  n = 10L,
  emotion = "happy",
  buffer = c(before = 2, after = 1),
  buffer_units = "seconds",
  output_path = "recording-shared-clips.zip"
)
```

`coded_data` must be an `fr_coding` object with `metadata$fps`.
`shared_synchrony` is the result from `shared_synchronous_episodes()`.
`video_paths` is a named character vector of existing video files; its
names must match the `id` values in the shared-synchrony table. `n`
selects the highest-ranked intervals, and `emotion` optionally filters
them before ranking.

`buffer` can be one non-negative value for a symmetric buffer or a named
vector such as `c(before = 2, after = 1)` for asymmetric buffering. Set
`buffer_units` to `"seconds"` or `"frames"`. The legacy `buffer_frames`
and `buffer_seconds` arguments remain available, but should not be
supplied together with `buffer`.

`output` can be `"zip"` or `"folder"`; ZIP output requires an `.zip`
`output_path`. If `output_path` is `NULL`, the output is placed beside
the selected source video, which requires all selected videos to be in
the same directory. `overwrite = TRUE` permits replacing an existing
archive or non-empty output folder. `ffmpeg` is either the FFmpeg
command available on `PATH` or an executable path.

The manifest records the selected intervals, frame and time boundaries,
source video paths, generated clip filenames, and output paths. Frame
ranges are inclusive, so a one-frame interval has duration `1 / fps`.

### `reaction_rate()`

`reaction_rate()` estimates the proportion of eligible episodes that
contain at least one delta reaction after an initial exclusion window.

``` r
library(facereaderconverter)

reaction_rate(coded_data, fps = 30)
```

This is useful when you want a simple summary of reaction frequency by
subject and emotion.

**Arguments:** `coded_data` must contain the episode and delta tables
produced by `convert_to_episodes()`. `time_limit` is the maximum window
length in seconds. `time_limit_frames`, when supplied, overrides
`time_limit`. `constraint_method` determines the window endpoint:
`"strict"` uses the earlier endpoint, `"episode"` uses the observed
episode endpoint, `"loose"` uses the later endpoint, and `"frames"` uses
only the requested limit. `exclude_start` omits reactions during the
initial delay in seconds; `exclude_start_frames` overrides it when
supplied. `fps` converts seconds to frames when needed. `subject_names`
optionally restricts the compared subjects; `NULL` uses all subjects.
`exclude_emotions` omits denominator emotions. `minimum_threshold`
requires this proportion of non-missing numerator values in the
constrained window.

### `reaction_rate_by_episode()`

`reaction_rate_by_episode()` returns the episode-level table used by
`reaction_rate()`. It includes one row per denominator episode with the
matched numerator subject, the episode window, the number of valid
frames, and a logical `reaction` flag.

``` r
library(facereaderconverter)

reaction_rate_by_episode(coded_data, fps = 30)
```

Use this when you need to inspect which specific episodes contributed to
the aggregated reaction-rate summary.

**Arguments:** `coded_data`, `time_limit`, `time_limit_frames`,
`constraint_method`, `exclude_start`, `exclude_start_frames`, `fps`,
`subject_names`, `exclude_emotions`, and `minimum_threshold` have the
same meanings as in `reaction_rate()`. The result retains one row per
denominator episode so the window and reaction decision can be
inspected.

### `negative_controls()`

`negative_controls()` samples control intervals matching the inclusive
frame lengths of supplied episodes, then calculates episode-level
synchrony for those intervals. Candidate controls avoid all supplied
episodes and previously accepted controls for the same ID. Each source
episode is marked as matched or unmatched if no valid interval is found
within `max_tries` attempts. Because control selection is random, set a
seed before calling the function for reproducible results.

``` r
library(facereaderconverter)

source_episodes <- synchrony_by_episode(coded_data)
controls <- negative_controls(coded_data, source_episodes)
```

`coded_data` is the `fr_coding` object returned by
`convert_to_episodes()`. `episodes` is the source episode table,
typically from `synchrony_by_episode()`. `max_tries` limits random
candidate starts per source episode. The result includes control frame
bounds, matching status, and the synchrony fields from
`synchrony_by_episode()`.

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

**Arguments:** `coding` is either a coding table or an `fr_coding`
object. `fps` is the sampling rate used for episode durations; an
`fr_coding` object’s stored value takes precedence.
`consecutive_missing` limits how far LOCF assignments may continue after
the last in-state frame; `NULL` uses the value stored in an `fr_coding`
object, or leaves the limit unset for a raw table.

## Utilities

### `to_seconds()`

`to_seconds()` converts FaceReader-style timestamps such as `hh:mm:ss`
or `hh:mm:ss.mmm` into numeric seconds.

``` r
library(facereaderconverter)

c(
  to_seconds("00:00:10", digits = 0L),
  to_seconds("00:00:10.500"),
  to_seconds("00:01:00", digits = 0L)
)
#> [1] 10.0 10.5 60.0
```

Use this when you need a numeric time variable for plotting or joining.

**Arguments:** `x` is a character vector of `hh:mm:ss` or
`hh:mm:ss.fraction` values. `digits` is the non-negative number of
fractional digits to retain; fractional values are truncated or
right-padded, and `0` discards them.

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

**Arguments:** `video_time` is a character or numeric vector containing
`HH:MM:SS`, `MM:SS`, or seconds. `fps` is the sampling rate; each result
is `round(seconds * fps)` and returned as an integer frame index.

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

**Arguments:** `input_dir` is the source root, `output_dir` is the
destination root, and `files` is a character vector of paths under
`input_dir`. The relative structure beneath `input_dir` is preserved,
destination directories are created as needed, and paths outside the
source root cause an error.

This is useful when converting a directory of files into a parallel
output tree.

## Exported function dependencies

Most exported functions are independent helpers, but several depend on
other exported functions or on custom structures produced by other
functions. A direct call is not always the same as a workflow
dependency, so both are listed separately.

| Exported function | Direct exported-function dependencies | Required structure/class or input shape | Common upstream producer(s) |
|----|----|----|----|
| `loadFRfile()` | `convertFRFiles()`, `convertFRExcelFiles()` | none | none |
| `convertFRDirectory()` | `convertFRFiles()`, `map_paths()` | none | none |
| `convert_to_episodes()` | `parse_time_to_frame()` | creates `fr_coding` | none |
| `add_delta_column()` | `parse_time_to_frame()` | accepts `fr_coding`; otherwise needs a coding table with `id`, `subject`, `emotion`, `frame` or `video_time`, and `value` | `convert_to_episodes()` |
| `delta_episodes()` | none | needs `delta` plus `id`, `subject`, `emotion`, `frame` (and optionally `video_time`, `value`) | `add_delta_column()` |
| `locf()` | none | accepts `fr_coding`; otherwise needs a coding table with `id`, `subject`, `emotion`, `video_time`, `value`, and `run_id` | `convert_to_episodes()` |
| `reaction_rate()` | none | accepts `fr_coding`; otherwise needs a delta-coded table with `id`, `subject`, `emotion`, `frame`, `delta`, `run_id`, and `in_state` | `add_delta_column()` |
| `reaction_rate_by_episode()` | none | accepts `fr_coding`; otherwise needs a delta-coded table with `id`, `subject`, `emotion`, `frame`, `delta`, `run_id`, and `in_state` | `add_delta_column()` |
| `negative_controls()` | `synchrony_by_episode()` | requires `fr_coding` and an episode table | `convert_to_episodes()`, `synchrony_by_episode()` |
| `synchrony_moments_pipeline()` | `convert_to_episodes()`, `shared_synchronous_episodes()`, `export_shared_synchrony_clips()` | input directory containing paired detailed exports and videos | none |
| `synchrony()` | none | requires `fr_coding` returned by `convert_to_episodes()` | `convert_to_episodes()` |
| `synchrony_by_episode()` | none | requires `fr_coding` returned by `convert_to_episodes()` | `convert_to_episodes()` |
| `shared_synchronous_episodes()` | none | requires `fr_coding` with episode `max_value` | `convert_to_episodes()` |
| `export_shared_synchrony_clips()` | none | requires `fr_coding` with `metadata$fps`, shared-synchrony table, named video paths, and FFmpeg | `shared_synchronous_episodes()` |
| `convertFRFiles()` | none | none | none |
| `convertFRExcelFiles()` | none | none | none |
| `to_seconds()` | none | none | none |
| `parse_time_to_frame()` | none | none | none |
| `map_paths()` | none | none | none |
