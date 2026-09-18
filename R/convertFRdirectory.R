#' Convert a directory of Facereader files to CSV
#'
#' Reads TXT, XLSX, and CSV files in a folder and converts them to CSV.

#' @param inpath Path to an existing directory.
#' @param outpath Path to save the csvs to defaults to the inpath
#' @param recursive Bool as to whether to look for all files in directory (`TRUE`) or just the root folder (`FALSE`)
#' @param pattern a regex pattern of files to test, if `NULL` then will look for TXT, XLSX, and CSV files
#' @param values_as_numeric Save values as numeric, where applicable
#' @param clean_names returns janitor-style clean names
#' @param fail_codes adds a column with the fail reason, True or False. Column then has 0 for success, 1 for fit_failed, 2 for find_failed
#' @param duplicate_timecodes_as_error throws an error if there are duplicate timecodes, if FALSE then throws warning
#' @param id Optional scalar ID or function of each input path returning one.
#' @param subject Optional scalar subject or function of each input path returning one.
#' @param save_metadata save the metadata as a csv in the outpath, set to NULL to not save
#' @param metadata_filename filename of the metadata csv
#' @param cores integer Number of threads to use. Default 0 is auto.
#' @param ... arguments passed as necessary
#' @return Invisibly returns the metadata.
#' @examples
#' \dontrun{
#' convertFRDirectory(
#'   inpath="directory_of_txt_files",
#'   outpath="directory_to_save_csvs_to",
#'   values_as_numeric = TRUE
#' )
#' }
#' @export
#'
#' @importFrom dplyr across
#' @importFrom tibble as_tibble
#' @importFrom stats "time"

convertFRDirectory <- function(
  inpath,
  outpath = inpath,
  recursive = TRUE,
  pattern = NULL,
  values_as_numeric = TRUE,
  clean_names = TRUE,
  save_metadata = outpath,
  metadata_filename = "metadata.csv",
  fail_codes = FALSE,
  duplicate_timecodes_as_error = TRUE,
  id = NULL,
  subject = NULL,
  cores = 0L,
  ...
) {
  ls <- list.files(
    inpath,
    recursive = recursive,
    full.names = TRUE
  )
  extension_pattern <- "\\.(txt|xlsx|csv)$"
  if (is.null(pattern)) {
    ls <- ls[grepl(extension_pattern, ls, ignore.case = TRUE)]
  } else {
    ls <- ls[
      grepl(pattern, basename(ls)) &
        grepl(extension_pattern, ls, ignore.case = TRUE)
    ]
  }

  # Do not rediscover CSVs generated from TXT or XLSX inputs.
  source_files <- ls[
    tolower(tools::file_ext(ls)) %in% c("txt", "xlsx")
  ]
  source_stems <- tools::file_path_sans_ext(basename(source_files))
  csv_stems <- tools::file_path_sans_ext(basename(ls))
  is_derived_csv <- tolower(tools::file_ext(ls)) == "csv" &
    csv_stems %in% source_stems
  ls <- ls[!is_derived_csv]
  ls <- ls[!grepl("^metadata.*\\.csv$", basename(ls), ignore.case = TRUE)]
  if (outpath != inpath) {
    output_dir <- normalizePath(outpath, winslash = "/", mustWork = FALSE)
    input_files <- normalizePath(ls, winslash = "/", mustWork = FALSE)
    is_in_output_dir <- startsWith(
      input_files,
      paste0(output_dir, "/")
    )
    ls <- ls[!is_in_output_dir]
  }

  # initialise metadata with time as POSIXct
  metadata_template <- tibble::tibble(
    inpath = character(),
    outpath = character(),
    video_filename = character(),
    time = as.POSIXct(character(), tz = "UTC"),
    type = character(),
    status = character(),
    error = character()
  )

  if (outpath != inpath) {
    ls_out <- map_paths(inpath, outpath, ls)
  } else {
    ls_out <- ls
  }

  if (!is.numeric(cores) || length(cores) != 1L || is.na(cores)) {
    stop("`cores` must be a single numeric value.")
  }
  cores <- as.integer(cores)
  if (cores < 0L) {
    stop("`cores` must be non-negative.")
  }
  if (cores == 0L) {
    cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
  }

  converter <- convertFRFiles
  loader <- loadFRfile

  preflight_indices <- seq_along(ls)
  preflight_data <- lapply(
    preflight_indices,
    function(i) {
      tryCatch(
        suppressWarnings(loader(
          ls[i],
          values_as_numeric = values_as_numeric,
          clean_names = clean_names,
          fail_codes = fail_codes,
          duplicate_timecodes_as_error = duplicate_timecodes_as_error,
          id = id,
          subject = subject,
          ...
        )),
        error = identity
      )
    }
  )
  preflight_output_paths <- vapply(
    preflight_indices,
    function(i) {
      data <- preflight_data[[i]]
      if (inherits(data, "condition") || is.null(data)) {
        return(NA_character_)
      }
      fr_output_path(
        ls_out[i],
        resolve_conversion_metadata(id, subject, ls[i]),
        fr_conversion_type(data)
      )
    },
    character(1)
  )
  comparable_paths <- normalizePath(
    preflight_output_paths,
    winslash = "/",
    mustWork = FALSE
  )
  if (.Platform$OS.type == "windows") {
    comparable_paths <- tolower(comparable_paths)
  }
  duplicate_paths <- comparable_paths[!is.na(comparable_paths)]
  duplicate_paths <- unique(duplicate_paths[duplicated(duplicate_paths)])
  for (duplicate_path in duplicate_paths) {
    duplicate_indices <- preflight_indices[comparable_paths == duplicate_path]
    duplicate_extensions <- tolower(tools::file_ext(ls[duplicate_indices]))
    if (!all(duplicate_extensions == "txt")) {
      stop(
        "Multiple inputs resolve to the same output destination: ",
        paste(basename(ls[duplicate_indices]), collapse = ", ")
      )
    }
  }

  process_file <- function(i) {
    warning_message <- NULL
    success <- TRUE
    md <- tryCatch(
      withCallingHandlers(
        {
          if (tolower(tools::file_ext(ls[i])) == "xlsx") {
            data <- preflight_data[[i]]
            if (inherits(data, "condition") || is.null(data)) {
              stop(data)
            }
            csv_path <- fr_output_path(
              ls_out[i],
              resolve_conversion_metadata(id, subject, ls[i]),
              fr_conversion_type(data)
            )
            readr::write_csv(data, csv_path)
            data.frame(
              video_filename = NA_character_,
              time = as.POSIXct(NA, tz = "UTC"),
              type = NA_character_,
              inpath = ls[i],
              outpath = csv_path
            )
          } else if (tolower(tools::file_ext(ls[i])) == "csv") {
            data <- preflight_data[[i]]
            if (inherits(data, "condition") || is.null(data)) {
              stop(data)
            }
            csv_path <- fr_output_path(
              ls_out[i],
              resolve_conversion_metadata(id, subject, ls[i]),
              fr_conversion_type(data)
            )
            readr::write_csv(data, csv_path)
            data.frame(
              video_filename = NA_character_,
              time = as.POSIXct(NA, tz = "UTC"),
              type = NA_character_,
              inpath = ls[i],
              outpath = csv_path
            )
          } else {
            converter(
              ls[i],
              outpath = ls_out[i],
              values_as_numeric = values_as_numeric,
              clean_names = clean_names,
              fail_codes = fail_codes,
              duplicate_timecodes_as_error = duplicate_timecodes_as_error,
              id = id,
              subject = subject,
              ...
            )
          }
        },
        warning = function(w) {
          warning_message <<- conditionMessage(w)
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        success <<- FALSE
        # use POSIXct NA for time and character NA for strings
        tibble::tibble(
          video_filename = NA_character_,
          time = as.POSIXct(NA, tz = "UTC"),
          type = NA_character_,
          inpath = as.character(ls[i]),
          outpath = as.character(ls_out[i]),
          status = "Fail",
          error = as.character(e$message)
        )
      }
    )

    if (success) {
      if (!is.null(warning_message)) {
        warning(warning_message, call. = FALSE)
      }

      # coerce success-row types to match metadata
      md |>
        dplyr::mutate(
          video_filename = as.character(video_filename),
          time = as.POSIXct(time, tz = "UTC"),
          type = as.character(type),
          inpath = as.character(inpath),
          outpath = as.character(outpath),
          status = "Success",
          error = NA_character_
        )
    } else {
      md
    }
  }

  if (length(ls) == 0L) {
    metadata <- metadata_template
  } else if (cores > 1L && length(ls) > 1L) {
    worker_count <- min(cores, length(ls))
    metadata <- tryCatch(
      {
        cl <- parallel::makeCluster(worker_count)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        parallel::clusterEvalQ(cl, {
          library(facereaderconverter)
          NULL
        })
        parallel::clusterExport(
          cl,
          c(
            "resolve_conversion_metadata",
            "add_fr_metadata",
            "fr_conversion_type",
            "fr_output_path",
            "converter",
            "loader",
            "preflight_data"
          ),
          envir = environment()
        )
        dplyr::bind_rows(parallel::parLapplyLB(
          cl,
          seq_along(ls),
          process_file
        ))
      },
      error = function(e) {
        dplyr::bind_rows(lapply(seq_along(ls), process_file))
      }
    )
  } else {
    metadata <- dplyr::bind_rows(lapply(seq_along(ls), process_file))
  }

  if (!is.null(save_metadata)) {
    dir.create(save_metadata, showWarnings = FALSE, recursive = TRUE)
    utils::write.csv(metadata, file.path(save_metadata, metadata_filename))
  }

  message(
    "Successfully converted ",
    sum(metadata$status == "Success"),
    " files."
  )
  invisible(metadata)
}
