#' Export clips for shared synchronous episodes
#'
#' Selects shared synchronous intervals with the largest combined emotion values,
#' or a named subject's highest-valued shared or individual episodes, and exports
#' the corresponding video segments with FFmpeg. Frame ranges are inclusive: a
#' range from frame 0 through frame 0 has duration `1 / fps`.
#'
#' @param coded_data A converted `fr_coding` object with `metadata$fps`.
#' @param shared_synchrony A table returned by [shared_synchronous_episodes()].
#' @param video_paths Named character vector of source video paths. Names must
#'   match values in the shared-synchrony `id` column.
#' @param n Number of highest-ranked intervals to export. Use `0` to export all
#'   intervals remaining after filtering.
#' @param emotion Optional character vector of emotions to retain.
#' @param optimised_subject Subject-name regular expression used to rank clips by
#'   one subject's maximum emotion value. Defaults to `"both"`, which ranks shared
#'   synchronies by their combined value.
#' @param only_synchronies Logical; when `TRUE`, select shared synchronies. When
#'   `FALSE`, select individual episodes for `optimised_subject`. Ignored when
#'   `optimised_subject = "both"`.
#' @param buffer A non-negative number for a symmetric buffer, or a named
#'   two-element vector with names `before` and `after` for asymmetric buffers.
#'   Units are controlled by `buffer_units`. Defaults to 5 seconds before and
#'   3 seconds after. Use `NULL` internally to select these defaults while
#'   preserving the legacy buffer arguments.
#' @param buffer_units Buffer units: `"seconds"` or `"frames"`.
#' @param buffer_frames Non-negative whole-number symmetric frame buffer. Kept
#'   for compatibility; use `buffer` and `buffer_units` for new code.
#' @param buffer_seconds Non-negative symmetric time buffer in seconds. Kept
#'   for compatibility; use `buffer` and `buffer_units` for new code.
#' @param output_path Optional ZIP file path when `output = "zip"`, or output
#'   directory when `output = "folder"`. By default, output is placed beside the
#'   selected source video. All selected videos must then be in the same folder.
#' @param output Output type: `"zip"` (the default) or `"folder"`.
#' @param overwrite Logical; overwrite an existing ZIP archive or non-empty
#'   output folder. Defaults to `FALSE`.
#' @param ffmpeg Path to the FFmpeg executable or its command name.
#' @param verbose Whether to display FFmpeg output instead of a clip progress bar.
#'
#' @return A `data.table` manifest invisibly. For ZIP output, the manifest is
#'   also included in the archive as `manifest.csv`.
#' @examples
#' \dontrun{
#' coded_data <- convert_to_episodes(coding_data)
#' shared <- shared_synchronous_episodes(coded_data)
#' export_shared_synchrony_clips(
#'   coded_data,
#'   shared,
#'   video_paths = c("1" = "recording.mp4"),
#'   n = 10,
#'   optimised_subject = "child",
#'   only_synchronies = FALSE,
#'   buffer = c(before = 2, after = 1),
#'   output_path = "recording-shared-clips.zip"
#' )
#' }
#' @export
export_shared_synchrony_clips <- function(
  coded_data,
  shared_synchrony,
  video_paths,
  n = 10L,
  emotion = "happy",
  optimised_subject = "both",
  only_synchronies = TRUE,
  buffer = NULL,
  buffer_units = c("seconds", "frames"),
  buffer_frames = 0L,
  buffer_seconds = 0,
  output_path = NULL,
  output = c("zip", "folder"),
  overwrite = FALSE,
  ffmpeg = "ffmpeg",
  verbose = FALSE
) {
  manifest <- prepare_shared_synchrony_clips(
    coded_data = coded_data,
    shared_synchrony = shared_synchrony,
    video_paths = video_paths,
    n = n,
    emotion = emotion,
    optimised_subject = optimised_subject,
    only_synchronies = only_synchronies,
    buffer = buffer,
    buffer_units = buffer_units,
    buffer_frames = buffer_frames,
    buffer_seconds = buffer_seconds
  )
  output <- match.arg(output)
  if (is.null(output_path)) {
    video_dirs <- unique(dirname(manifest$video_path))
    if (length(video_dirs) != 1L) {
      stop(
        "`output_path` is required when selected videos are in different folders.",
        call. = FALSE
      )
    }
    video_stem <- tools::file_path_sans_ext(basename(manifest$video_path[[1L]]))
    output_path <- file.path(
      video_dirs,
      paste0(
        video_stem,
        "-shared-synchrony-clips",
        if (output == "zip") ".zip" else ""
      )
    )
  }
  if (
    !is.character(output_path) ||
      length(output_path) != 1L ||
      is.na(output_path) ||
      !nzchar(output_path)
  ) {
    stop("`output_path` must be NULL or one non-empty path.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  if (
    !is.character(ffmpeg) ||
      length(ffmpeg) != 1L ||
      is.na(ffmpeg) ||
      !nzchar(ffmpeg)
  ) {
    stop(
      "`ffmpeg` must be one non-empty executable path or command.",
      call. = FALSE
    )
  }
  ffmpeg_path <- Sys.which(ffmpeg)
  if (!nzchar(ffmpeg_path) && !file.exists(ffmpeg)) {
    stop(
      paste(
        "FFmpeg was not found.",
        "Install it from https://ffmpeg.org/download.html and add it to PATH,",
        "or supply its executable path with `ffmpeg`."
      ),
      call. = FALSE
    )
  }
  destination <- normalizePath(output_path, winslash = "/", mustWork = FALSE)
  if (output == "zip" && file.exists(destination) && !overwrite) {
    stop(
      "The ZIP archive already exists; use `overwrite = TRUE` to replace it.",
      call. = FALSE
    )
  }
  if (output == "folder") {
    source_paths <- normalizePath(
      manifest$video_path,
      winslash = "/",
      mustWork = TRUE
    )
    destination_prefix <- paste0(sub("/+$", "", destination), "/")
    compare_path <- if (.Platform$OS.type == "windows") tolower else identity
    source_in_destination <- vapply(
      source_paths,
      function(source_path) {
        source_path <- compare_path(source_path)
        destination_path <- compare_path(destination)
        destination_prefix_path <- compare_path(destination_prefix)
        identical(source_path, destination_path) ||
          startsWith(source_path, destination_prefix_path)
      },
      logical(1)
    )
    if (any(source_in_destination)) {
      stop(
        "The output directory cannot contain a selected source video.",
        call. = FALSE
      )
    }
  }
  ffprobe_path <- Sys.which("ffprobe")
  if (!nzchar(ffprobe_path)) {
    stop(
      "FFprobe was not found; it is required to determine source video durations.",
      call. = FALSE
    )
  }
  source_durations <- vapply(
    unique(manifest$video_path),
    probe_shared_synchrony_video_duration,
    numeric(1),
    ffprobe = ffprobe_path
  )
  names(source_durations) <- unique(manifest$video_path)
  manifest <- prepare_shared_synchrony_clips(
    coded_data = coded_data,
    shared_synchrony = shared_synchrony,
    video_paths = video_paths,
    n = n,
    emotion = emotion,
    optimised_subject = optimised_subject,
    only_synchronies = only_synchronies,
    buffer = buffer,
    buffer_units = buffer_units,
    buffer_frames = buffer_frames,
    buffer_seconds = buffer_seconds,
    video_durations = source_durations
  )
  if (output == "zip" && !grepl("\\.zip$", output_path, ignore.case = TRUE)) {
    stop(
      "`output_path` must end in `.zip` when `output = \"zip\"`.",
      call. = FALSE
    )
  }

  staging_dir <- if (output == "folder") {
    destination
  } else {
    tempfile("shared-synchrony-clips-")
  }
  if (dir.exists(staging_dir)) {
    if (length(list.files(staging_dir, all.files = TRUE, no.. = TRUE)) > 0L) {
      if (!overwrite) {
        stop(
          "The output directory is not empty; use `overwrite = TRUE` to replace it.",
          call. = FALSE
        )
      }
      unlink(staging_dir, recursive = TRUE, force = TRUE)
    }
  }
  if (!dir.exists(staging_dir) && !dir.create(staging_dir, recursive = TRUE)) {
    stop("Could not create the output directory.", call. = FALSE)
  }
  if (output == "zip") {
    on.exit(unlink(staging_dir, recursive = TRUE, force = TRUE), add = TRUE)
  }

  progress <- if (!verbose && nrow(manifest) > 0L) {
    utils::txtProgressBar(min = 0L, max = nrow(manifest), style = 3L)
  } else {
    NULL
  }
  if (!is.null(progress)) {
    on.exit(close(progress), add = TRUE)
  }
  for (i in seq_len(nrow(manifest))) {
    clip_path <- file.path(staging_dir, manifest$clip_filename[[i]])
    args <- c(
      "-y",
      "-i",
      shQuote(manifest$video_path[[i]]),
      "-ss",
      sprintf("%.9f", manifest$start_seconds[[i]]),
      "-t",
      sprintf("%.9f", manifest$duration_seconds[[i]]),
      "-map",
      "0:v:0",
      "-map",
      "0:a:0?",
      "-c:v",
      "libx264",
      "-c:a",
      "aac",
      shQuote(clip_path)
    )
    if (!verbose) {
      args <- c("-loglevel", "error", "-nostats", args)
    }
    executable <- if (nzchar(ffmpeg_path)) ffmpeg_path else ffmpeg
    status <- if (verbose) {
      system2(executable, args = args)
    } else {
      system2(executable, args = args, stdout = FALSE, stderr = FALSE)
    }
    if (
      (!is.null(status) && status != 0L) ||
        !shared_synchrony_file_exists(clip_path)
    ) {
      stop(sprintf("FFmpeg failed while creating clip %d.", i), call. = FALSE)
    }
    if (!is.null(progress)) {
      utils::setTxtProgressBar(progress, i)
    }
    manifest$clip_path[[i]] <- normalizePath(
      clip_path,
      winslash = "/",
      mustWork = FALSE
    )
  }
  if (output == "zip") {
    manifest[, `:=`(clip_path = clip_filename, archive_path = destination)]
  } else {
    manifest[, archive_path := NA_character_]
  }
  manifest_path <- file.path(staging_dir, "manifest.csv")
  data.table::fwrite(manifest, manifest_path)

  if (output == "zip") {
    parent <- dirname(destination)
    if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE)) {
      stop("Could not create the ZIP output directory.", call. = FALSE)
    }
    if (file.exists(destination) && unlink(destination) != 0L) {
      stop("Could not replace the existing ZIP archive.", call. = FALSE)
    }
    old_dir <- getwd()
    on.exit(setwd(old_dir), add = TRUE)
    setwd(staging_dir)
    utils::zip(destination, files = c(manifest$clip_filename, "manifest.csv"))
    if (!file.exists(destination)) {
      stop("Could not create the ZIP archive.", call. = FALSE)
    }
  }
  invisible(manifest)
}

shared_synchrony_file_exists <- function(path) {
  if (file.exists(path)) {
    return(TRUE)
  }
  if (.Platform$OS.type != "windows") {
    return(FALSE)
  }
  long_path <- paste0(
    "\\\\?\\",
    normalizePath(path, winslash = "\\", mustWork = FALSE)
  )
  file.exists(long_path)
}

probe_shared_synchrony_video_duration <- function(path, ffprobe) {
  duration <- suppressWarnings(as.numeric(system2(
    ffprobe,
    args = c(
      "-v",
      "error",
      "-show_entries",
      "format=duration",
      "-of",
      "default=noprint_wrappers=1:nokey=1",
      shQuote(path)
    ),
    stdout = TRUE,
    stderr = FALSE
  )))
  if (
    length(duration) != 1L ||
      is.na(duration) ||
      !is.finite(duration) ||
      duration <= 0
  ) {
    stop(
      sprintf("Could not determine the duration of source video `%s`.", path),
      call. = FALSE
    )
  }
  duration
}

prepare_shared_synchrony_clips <- function(
  coded_data,
  shared_synchrony,
  video_paths,
  n,
  emotion,
  optimised_subject = "both",
  only_synchronies = TRUE,
  buffer = NULL,
  buffer_units = c("seconds", "frames"),
  buffer_frames = 0L,
  buffer_seconds = 0,
  video_durations = NULL
) {
  if (!inherits(coded_data, "fr_coding") || is.null(coded_data$metadata$fps)) {
    stop(
      "`coded_data` must be an `fr_coding` object with `metadata$fps`.",
      call. = FALSE
    )
  }
  fps <- coded_data$metadata$fps
  if (!is.numeric(fps) || length(fps) != 1L || is.na(fps) || fps <= 0) {
    stop("`coded_data$metadata$fps` must be a positive number.", call. = FALSE)
  }
  required_shared <- c(
    "id",
    "emotion",
    "subject1",
    "subject2",
    "subject1_run_id",
    "subject2_run_id",
    "start_frame",
    "end_frame",
    "combined_value"
  )
  if (
    !is.data.frame(shared_synchrony) ||
      !all(required_shared %in% names(shared_synchrony))
  ) {
    stop(
      "`shared_synchrony` is missing required shared-episode columns.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(n) ||
      length(n) != 1L ||
      is.na(n) ||
      !is.finite(n) ||
      n < 0 ||
      n != floor(n) ||
      n > .Machine$integer.max
  ) {
    stop("`n` must be a non-negative whole number.", call. = FALSE)
  }
  if (
    !is.character(optimised_subject) ||
      length(optimised_subject) != 1L ||
      is.na(optimised_subject) ||
      !nzchar(optimised_subject)
  ) {
    stop(
      "`optimised_subject` must be one non-empty character string.",
      call. = FALSE
    )
  }
  if (
    !identical(optimised_subject, "both") &&
      (!is.logical(only_synchronies) ||
        length(only_synchronies) != 1L ||
        is.na(only_synchronies))
  ) {
    stop("`only_synchronies` must be TRUE or FALSE.", call. = FALSE)
  }
  buffer_units <- match.arg(buffer_units)
  buffer_supplied <- !is.null(buffer)
  if (buffer_supplied) {
    if (
      !is.numeric(buffer) ||
        anyNA(buffer) ||
        any(!is.finite(buffer)) ||
        any(buffer < 0)
    ) {
      stop("`buffer` must contain finite non-negative numbers.", call. = FALSE)
    }
    if (length(buffer) == 1L) {
      buffer <- c(before = buffer, after = buffer)
    } else if (
      length(buffer) == 2L &&
        !is.null(names(buffer)) &&
        identical(names(buffer), c("before", "after"))
    ) {
      buffer <- unname(buffer)
      names(buffer) <- c("before", "after")
    } else {
      stop(
        "`buffer` must be one number or a named vector with `before` and `after`.",
        call. = FALSE
      )
    }
  }
  if (
    !is.numeric(buffer_frames) ||
      length(buffer_frames) != 1L ||
      is.na(buffer_frames) ||
      buffer_frames < 0 ||
      buffer_frames != as.integer(buffer_frames)
  ) {
    stop("`buffer_frames` must be a non-negative whole number.", call. = FALSE)
  }
  if (
    !is.numeric(buffer_seconds) ||
      length(buffer_seconds) != 1L ||
      is.na(buffer_seconds) ||
      !is.finite(buffer_seconds) ||
      buffer_seconds < 0
  ) {
    stop(
      "`buffer_seconds` must be a finite non-negative number.",
      call. = FALSE
    )
  }
  if (buffer_frames > 0 && buffer_seconds > 0) {
    stop(
      "Supply either `buffer_frames` or `buffer_seconds`, not both.",
      call. = FALSE
    )
  }
  if (buffer_frames > 0 || buffer_seconds > 0) {
    if (buffer_supplied && any(buffer != 0)) {
      stop(
        "Supply either `buffer` or the legacy buffer arguments, not both.",
        call. = FALSE
      )
    }
    if (buffer_frames > 0) {
      buffer <- c(before = buffer_frames, after = buffer_frames)
      buffer_units <- "frames"
    } else {
      buffer <- c(before = buffer_seconds, after = buffer_seconds)
      buffer_units <- "seconds"
    }
  } else if (!buffer_supplied) {
    buffer <- c(before = 5, after = 3)
  }
  if (!is.null(emotion) && (!is.character(emotion) || anyNA(emotion))) {
    stop(
      "`emotion` must be NULL or a character vector without missing values.",
      call. = FALSE
    )
  }
  if (
    !is.character(video_paths) ||
      is.null(names(video_paths)) ||
      anyNA(names(video_paths)) ||
      any(!nzchar(names(video_paths))) ||
      anyDuplicated(names(video_paths))
  ) {
    stop(
      "`video_paths` must be a named character vector with unique ID names.",
      call. = FALSE
    )
  }
  if (anyNA(video_paths) || any(!file.exists(video_paths))) {
    stop(
      "Every `video_paths` entry must name an existing video file.",
      call. = FALSE
    )
  }
  if (!is.null(video_durations)) {
    if (
      !is.numeric(video_durations) ||
        is.null(names(video_durations)) ||
        anyNA(video_durations) ||
        any(!is.finite(video_durations)) ||
        any(video_durations <= 0) ||
        !all(
          normalizePath(video_paths, winslash = "/", mustWork = TRUE) %in%
            names(video_durations)
        )
    ) {
      stop(
        "`video_durations` must contain positive durations for every video.",
        call. = FALSE
      )
    }
  }

  shared_clips <- data.table::as.data.table(data.table::copy(shared_synchrony))
  if (identical(optimised_subject, "both")) {
    clips <- shared_clips
    clips[, selection_value := combined_value]
    clip_type <- "synchrony"
  } else if (only_synchronies) {
    subject_names <- unique(c(shared_clips$subject1, shared_clips$subject2))
    matched_subjects <- subject_names[grepl(optimised_subject, subject_names)]
    if (length(matched_subjects) != 1L) {
      stop(
        "`optimised_subject` must match exactly one subject name.",
        call. = FALSE
      )
    }
    required_values <- c("subject1_max_value", "subject2_max_value")
    if (!all(required_values %in% names(shared_clips))) {
      stop(
        "`shared_synchrony` is missing required subject maximum-value columns.",
        call. = FALSE
      )
    }
    clips <- shared_clips[
      subject1 == matched_subjects | subject2 == matched_subjects
    ]
    clips[,
      selection_value := data.table::fifelse(
        subject1 == matched_subjects,
        subject1_max_value,
        subject2_max_value
      )
    ]
    clip_type <- "synchrony"
  } else {
    required_episodes <- c(
      "id",
      "subject",
      "emotion",
      "start_frame",
      "end_frame",
      "run_id",
      "max_value"
    )
    if (
      is.null(coded_data$episodes) ||
        !is.data.frame(coded_data$episodes) ||
        !all(required_episodes %in% names(coded_data$episodes))
    ) {
      stop(
        "`coded_data$episodes` is missing required episode columns.",
        call. = FALSE
      )
    }
    episodes <- data.table::as.data.table(data.table::copy(coded_data$episodes))
    subject_names <- unique(episodes$subject)
    matched_subjects <- subject_names[grepl(optimised_subject, subject_names)]
    if (length(matched_subjects) != 1L) {
      stop(
        "`optimised_subject` must match exactly one subject name.",
        call. = FALSE
      )
    }
    clips <- episodes[subject == matched_subjects]
    data.table::setnames(
      clips,
      c("subject", "run_id"),
      c("subject1", "subject1_run_id")
    )
    clips[, `:=`(
      subject2 = NA_character_,
      subject2_run_id = NA_integer_,
      combined_value = max_value,
      selection_value = max_value
    )]
    clip_type <- "episode"
  }
  if (!is.null(emotion)) {
    selected_emotions <- emotion
    clips <- clips[emotion %in% selected_emotions]
  }
  if (nrow(clips) == 0L) {
    stop("No intervals remain after filtering.", call. = FALSE)
  }
  if (n == 0L) {
    n <- nrow(clips)
  } else {
    n <- min(as.integer(n), nrow(clips))
  }
  if (
    anyNA(clips$start_frame) ||
      anyNA(clips$end_frame) ||
      any(clips$start_frame < 0) ||
      any(clips$end_frame < clips$start_frame)
  ) {
    stop(
      "Interval frame bounds must be non-missing, non-negative, and ordered.",
      call. = FALSE
    )
  }
  data.table::setorderv(
    clips,
    c(
      "selection_value",
      "id",
      "emotion",
      "subject1",
      "subject2",
      "subject1_run_id",
      "subject2_run_id",
      "start_frame",
      "end_frame"
    ),
    c(-1L, rep(1L, 8L)),
    na.last = TRUE
  )
  clips <- clips[seq_len(n)]
  id_keys <- as.character(clips$id)
  missing_ids <- setdiff(unique(id_keys), names(video_paths))
  if (length(missing_ids) > 0L) {
    stop("`video_paths` is missing one or more selected IDs.", call. = FALSE)
  }
  frame_buffers <- if (buffer_units == "seconds") {
    as.integer(round(buffer * fps))
  } else {
    as.integer(buffer)
  }
  names(frame_buffers) <- c("before", "after")
  clips[, `:=`(
    selection_rank = seq_len(.N),
    original_start_frame = as.integer(start_frame),
    original_end_frame = as.integer(end_frame),
    start_frame = pmax(0L, as.integer(start_frame) - frame_buffers[["before"]]),
    end_frame = as.integer(end_frame) + frame_buffers[["after"]],
    fps = as.numeric(fps)
  )]
  if (!is.null(video_durations)) {
    clips[,
      end_frame := pmin(
        end_frame,
        as.integer(
          ceiling(
            video_durations[
              normalizePath(
                unname(video_paths[as.character(id)]),
                winslash = "/",
                mustWork = TRUE
              )
            ] *
              fps
          ) -
            1L
        )
      )
    ]
  }
  if (any(clips$end_frame < clips$start_frame)) {
    stop(
      "One or more selected intervals lie outside the source video duration.",
      call. = FALSE
    )
  }
  clips[, `:=`(
    start_seconds = start_frame / fps,
    duration_seconds = (end_frame - start_frame + 1) / fps,
    video_path = normalizePath(
      unname(video_paths[as.character(id)]),
      winslash = "/",
      mustWork = TRUE
    )
  )]
  clips[,
    clip_filename := if (clip_type == "synchrony") {
      sprintf(
        "%03d_id-%s_%s_runs-%s-%s_frames-%s-%s.mp4",
        selection_rank,
        gsub("[^[:alnum:]_-]", "_", as.character(id)),
        gsub("[^[:alnum:]_-]", "_", emotion),
        subject1_run_id,
        subject2_run_id,
        start_frame,
        end_frame
      )
    } else {
      sprintf(
        "%03d_id-%s_%s_subject-%s_run-%s_frames-%s-%s.mp4",
        selection_rank,
        gsub("[^[:alnum:]_-]", "_", as.character(id)),
        gsub("[^[:alnum:]_-]", "_", emotion),
        gsub("[^[:alnum:]_-]", "_", subject1),
        subject1_run_id,
        start_frame,
        end_frame
      )
    }
  ]
  clips[, c("clip_path", "archive_path") := list(NA_character_, NA_character_)]
  clips
}
