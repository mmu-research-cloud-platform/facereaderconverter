#' Convert a directory of detailed FaceReader exports to episodes
#'
#' Recursively loads detailed TXT and XLSX exports, combines their emotion
#' values, and calls [convert_to_episodes()] once. Each export's frame rate is
#' inherited from its header (non-integer rates such as 29.97 are kept) and
#' stored per `id` in `metadata$fps`; frame rates may differ between ids but
#' exports that resolve to the same `id` must agree. When a header has no
#' usable frame rate it is inferred from the time stamps with [infer_fps()]. State
#' exports are ignored. Returns the combined result in memory and saves it
#' as `coded_data` in one `.RDa` file.
#'
#' @param inpath Directory containing FaceReader exports.
#' @param outpath Path to the output `.RDa` file. Defaults to `episodes.RDa`
#'   inside `inpath`.
#' @param id_pattern,subject_pattern Optional regular expressions applied to the
#'   associated media filename and FaceReader export filename, respectively.
#'   If a rule is `NULL`, the complete corresponding basename without extension
#'   is used. A supplied `subject_pattern` must match every detailed export. An
#'   `id_pattern` (or media filename) that yields no match gives an `NA` ID;
#'   exports must still resolve to distinct ID and subject pairs, including
#'   `NA` IDs. Multiple ID matches in a media filename are rejected. Set
#'   `use_full_path = TRUE` to search the full media filename stored in
#'   metadata for IDs and the full FaceReader export path for subjects.
#' @param recursive Whether to search subdirectories.
#' @param overwrite Whether to replace an existing output file. If `FALSE`, an
#'   existing output triggers a warning and conversion is skipped. Defaults to
#'   `FALSE`.
#' @param skip_fails Whether to warn and skip individual exports that fail
#'   parsing, validation, or loading. Defaults to `TRUE`. An error is still
#'   raised if no detailed exports can be processed.
#' @param filter_name Optional regular expression matched against the export
#'   filename (including its extension), not its directory path.
#' @param T_up,T_down,delta,delta_window,min_dur_sec,consecutive_missing,cores
#'   Passed to [convert_to_episodes()].
#' @param use_full_path Search full metadata/export paths when a regex is
#'   supplied. Basename defaults are unchanged.
#'
#' @return The combined `fr_coding` object returned by
#'   [convert_to_episodes()], invisibly. The same object is saved as
#'   `coded_data` in `outpath`. If `outpath` already exists and `overwrite` is
#'   `FALSE`, loads and invisibly returns its saved `coded_data` without
#'   converting.
#' @examples
#' \dontrun{
#' coded_data <- convert_directory_to_episodes(
#'   "path/to/exports",
#'   id_pattern = "[0-9]{4}",
#'   subject_pattern = "mum|teen",
#'   filter_name = "_detailed\\.(txt|xlsx)$",
#'   skip_fails = TRUE
#' )
#' }
#' @export
convert_directory_to_episodes <- function(
  inpath,
  outpath = file.path(inpath, "episodes.RDa"),
  id_pattern = NULL,
  subject_pattern = NULL,
  recursive = TRUE,
  overwrite = FALSE,
  T_up = 0.20,
  T_down = 0.1,
  delta = 0.10,
  delta_window = 0.2,
  min_dur_sec = 0.1,
  consecutive_missing = 150L,
  cores = 0L,
  use_full_path = FALSE,
  skip_fails = TRUE,
  filter_name = NULL
) {
  if (
    !is.character(inpath) ||
      length(inpath) != 1L ||
      is.na(inpath) ||
      !dir.exists(inpath)
  ) {
    stop("`inpath` must be one existing directory.", call. = FALSE)
  }
  if (
    !is.character(outpath) ||
      length(outpath) != 1L ||
      is.na(outpath) ||
      !nzchar(outpath) ||
      !tolower(tools::file_ext(outpath)) %in% c("rda", "rdata")
  ) {
    stop("`outpath` must be a .RDa or .RData file path.", call. = FALSE)
  }
  if (!is.logical(recursive) || length(recursive) != 1L || is.na(recursive)) {
    stop("`recursive` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }
  if (
    !is.logical(skip_fails) || length(skip_fails) != 1L || is.na(skip_fails)
  ) {
    stop("`skip_fails` must be TRUE or FALSE.", call. = FALSE)
  }
  if (
    !is.null(filter_name) &&
      (!is.character(filter_name) ||
        length(filter_name) != 1L ||
        is.na(filter_name) ||
        !nzchar(filter_name))
  ) {
    stop("`filter_name` must be NULL or one non-empty regex.", call. = FALSE)
  }
  if (
    !is.logical(use_full_path) ||
      length(use_full_path) != 1L ||
      is.na(use_full_path)
  ) {
    stop("`use_full_path` must be TRUE or FALSE.", call. = FALSE)
  }
  if (file.exists(outpath) && !overwrite) {
    warning(
      "Output already exists: ",
      outpath,
      "; skipping conversion.",
      call. = FALSE
    )
    saved_data <- new.env(parent = emptyenv())
    load(outpath, envir = saved_data)
    return(invisible(saved_data$coded_data))
  }
  for (pattern in list(id_pattern, subject_pattern)) {
    if (
      !is.null(pattern) &&
        (!is.character(pattern) ||
          length(pattern) != 1L ||
          is.na(pattern) ||
          !nzchar(pattern))
    ) {
      stop(
        "ID and subject patterns must be NULL or one non-empty regex.",
        call. = FALSE
      )
    }
  }

  files <- sort(list.files(
    inpath,
    recursive = recursive,
    full.names = TRUE,
    pattern = "\\.(txt|xlsx)$",
    ignore.case = TRUE
  ))
  found_count <- length(files)
  message("Found ", found_count, " FaceReader TXT/XLSX file(s).")
  if (!is.null(filter_name)) {
    files <- files[grepl(filter_name, basename(files), perl = TRUE)]
    message(
      "Selected ",
      length(files),
      " file(s) with `filter_name`; filtered out ",
      found_count - length(files),
      " file(s)."
    )
  }

  emotion_columns <- c(
    "neutral",
    "happy",
    "sad",
    "angry",
    "surprised",
    "scared",
    "disgusted"
  )
  inputs <- list()
  accepted_files <- character()
  accepted_groups <- character()
  accepted_fps <- data.frame(
    id = character(),
    fps = numeric(),
    file = character(),
    source = character(),
    stringsAsFactors = FALSE
  )
  detailed_found <- FALSE
  non_detailed_count <- 0L
  failed_count <- 0L
  for (file in files) {
    process_file <- function() {
      header <- synchrony_fr_header_metadata(file)
      if (!identical(header$type, "detailed")) {
        non_detailed_count <<- non_detailed_count + 1L
        return(NULL)
      }
      detailed_found <<- TRUE
      # Inherit the frame rate from the file header; if the file has none it is
      # inferred from the time stamps once the data are loaded.
      fps <- header$fps
      fps_source <- "header"
      if (length(fps) != 1L || !is.finite(fps) || fps <= 0) {
        fps <- NA_real_
        fps_source <- "inferred"
      }
      media_filename <- header$video_filename
      media_name <- fr_media_id(media_filename)
      subject_name <- fr_filename_stem(file)
      if (use_full_path && !is.null(id_pattern)) {
        media_name <- media_filename
      }
      if (use_full_path && !is.null(subject_pattern)) {
        subject_name <- file
      }
      extract_one <- function(source, pattern, field) {
        if (is.null(source)) {
          return(NA_character_)
        }
        if (is.null(pattern)) {
          return(source)
        }
        matches <- unique(stringr::str_extract_all(source, pattern)[[1L]])
        matches <- matches[!is.na(matches) & nzchar(matches)]
        if (length(matches) > 1L && identical(field, "id")) {
          stop(
            "Multiple different IDs match `id_pattern` in media filename for: ",
            file,
            ": ",
            paste(matches, collapse = ", "),
            call. = FALSE
          )
        }
        if (length(matches)) matches[[1L]] else NA_character_
      }
      id <- extract_one(media_name, id_pattern, "id")
      subject <- extract_one(subject_name, subject_pattern, "subject")
      if (is.na(subject) || !nzchar(subject)) {
        stop("Subject pattern did not match: ", file, call. = FALSE)
      }
      group <- paste(id, subject, sep = "\r")
      if (group %in% accepted_groups) {
        duplicate <- accepted_files[match(group, accepted_groups)]
        stop(
          structure(
            class = c("fr_duplicate_export_error", "error", "condition"),
            list(
              message = paste0(
                "Multiple exports resolve to the same ID and subject: ",
                duplicate,
                ", ",
                file
              ),
              call = NULL
            )
          )
        )
      }
      data <- loadFRfile(file, values_as_numeric = TRUE, clean_names = TRUE)
      if (is.null(data) || !"video_time" %in% names(data)) {
        if (is.na(fps)) {
          stop("Invalid or missing frame rate in: ", file, call. = FALSE)
        }
        stop(
          "Missing detailed coding or video time in: ",
          file,
          call. = FALSE
        )
      }
      if (
        "participant_name" %in%
          names(data) &&
          length(unique(stats::na.omit(trimws(as.character(
            data$participant_name
          ))))) >
            1L
      ) {
        stop("Multiple participants in one export: ", file, call. = FALSE)
      }
      columns <- intersect(emotion_columns, names(data))
      if (!length(columns)) {
        stop("No emotion columns in: ", file, call. = FALSE)
      }
      input <- data.frame(
        id = id,
        subject = subject,
        video_time = data$video_time,
        data[, columns, drop = FALSE],
        check.names = FALSE
      )
      if (is.na(fps)) {
        inferred <- suppressWarnings(infer_fps(input))
        fps <- inferred$fps[[1L]]
        if (length(fps) != 1L || !is.finite(fps) || fps <= 0) {
          stop("Invalid or missing frame rate in: ", file, call. = FALSE)
        }
      }
      # Frame rates only have to agree within one id (one video)
      same_id <- if (is.na(id)) {
        is.na(accepted_fps$id)
      } else {
        !is.na(accepted_fps$id) & accepted_fps$id == as.character(id)
      }
      if (any(same_id)) {
        first <- accepted_fps[which(same_id)[[1L]], ]
        tolerance <- if (
          identical(fps_source, "header") &&
            identical(first$source, "header")
        ) {
          1e-8
        } else {
          0.005
        }
        if (abs(fps - first$fps) / first$fps > tolerance) {
          stop(
            "Conflicting frame rates in: ",
            first$file,
            " (",
            format(first$fps),
            " FPS), ",
            file,
            " (",
            format(fps),
            " FPS)",
            call. = FALSE
          )
        }
      }
      list(
        input = input,
        fps = fps,
        fps_source = fps_source,
        id = if (is.na(id)) NA_character_ else as.character(id),
        group = group
      )
    }
    processed <- if (skip_fails) {
      tryCatch(process_file(), error = function(error) {
        # Duplicate ID/subject pairs would collide in the output, so never skip them
        if (inherits(error, "fr_duplicate_export_error")) {
          stop(error)
        }
        failed_count <<- failed_count + 1L
        warning(
          "Skipping FaceReader export ",
          file,
          ": ",
          conditionMessage(error),
          call. = FALSE
        )
        NULL
      })
    } else {
      process_file()
    }
    if (is.null(processed)) {
      next
    }
    inputs[[length(inputs) + 1L]] <- processed$input
    accepted_files <- c(accepted_files, file)
    accepted_groups <- c(
      accepted_groups,
      if (is.null(processed$group)) NA_character_ else processed$group
    )
    accepted_fps <- rbind(
      accepted_fps,
      data.frame(
        id = processed$id,
        fps = processed$fps,
        file = file,
        source = processed$fps_source,
        stringsAsFactors = FALSE
      )
    )
  }
  message(
    "Processed ",
    length(inputs),
    " detailed file(s); ignored ",
    non_detailed_count,
    " non-detailed file(s); skipped ",
    failed_count,
    " failed file(s)."
  )
  if (!length(inputs)) {
    stop(
      if (detailed_found) {
        "No detailed FaceReader exports could be processed."
      } else {
        "No detailed FaceReader exports were found."
      },
      call. = FALSE
    )
  }
  coding_input <- dplyr::bind_rows(inputs)
  # One rate per id, inherited from each file's header (or inferred)
  fps_table <- accepted_fps[!duplicated(accepted_fps$id), c("id", "fps")]
  coded_data <- convert_to_episodes(
    coding_input,
    T_up = T_up,
    T_down = T_down,
    delta = delta,
    delta_window = delta_window,
    min_dur_sec = min_dur_sec,
    consecutive_missing = consecutive_missing,
    fps = fps_table,
    cores = cores
  )
  output_dir <- dirname(outpath)
  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist: ", output_dir, call. = FALSE)
  }
  save(coded_data, file = outpath)
  invisible(coded_data)
}
