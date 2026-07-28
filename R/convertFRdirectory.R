#' Convert a directory of Facereader TXT to CSV
#'
#' Reads all Txt files in a folder and sends them through convertFRFiles.

#' @param inpath Path to an existing .txt file.
#' @param outpath Path to save the csvs to defaults to the inpath
#' @param recursive Bool as to whether to look for all files in directory (`TRUE`) or just the root folder (`FALSE`)
#' @param pattern a regex pattern of files to test, if `NULL` then will look for all txt files
#' @param values_as_numeric Save values as numeric, where applicable
#' @param clean_names returns janitor-style clean names
#' @param fail_codes adds a column with the fail reason, True or False. Column then has 0 for success, 1 for fit_failed, 2 for find_failed
#' @param duplicate_timecodes_as_error throws an error if there are duplicate timecodes, if FALSE then throws warning
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
  cores = 0L,
  ...
) {
  if (is.null(pattern)) {
    ls <- list.files(
      inpath,
      pattern = ".*\\.txt$",
      recursive = recursive,
      full.names = TRUE
    )
  } else {
    ls <- list.files(
      inpath,
      pattern = paste0(pattern, ".*\\.txt$"),
      recursive = recursive,
      full.names = TRUE
    )
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

  process_file <- function(i) {
    tryCatch(
      {
        md <- convertFRFiles(
          ls[i],
          ls_out[i],
          values_as_numeric = values_as_numeric,
          clean_names = clean_names,
          fail_codes = fail_codes,
          duplicate_timecodes_as_error = duplicate_timecodes_as_error,
          ...
        )

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
      },
      error = function(e) {
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
      },
      warning = function(w) {
        # use POSIXct NA for time and character NA for strings
        tibble::tibble(
          video_filename = NA_character_,
          time = as.POSIXct(NA, tz = "UTC"),
          type = NA_character_,
          inpath = as.character(ls[i]),
          outpath = as.character(ls_out[i]),
          status = "Success",
          error = as.character(w$message)
        )
      }
    )
  }

  if (length(ls) == 0L) {
    metadata <- metadata_template
  } else if (cores > 1L && length(ls) > 1L) {
    worker_count <- min(cores, length(ls))
    cl <- parallel::makeCluster(worker_count)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, {
      library(facereaderconverter)
      NULL
    })
    metadata <- dplyr::bind_rows(parallel::parLapplyLB(
      cl,
      seq_along(ls),
      process_file
    ))
  } else {
    metadata <- dplyr::bind_rows(lapply(seq_along(ls), process_file))
  }

  if (!is.null(save_metadata)) {
    write.csv(metadata, paste0(save_metadata, "/", metadata_filename))
  }
  invisible(metadata)
}
