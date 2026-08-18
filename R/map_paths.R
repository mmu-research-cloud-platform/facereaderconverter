#' Map files from an input root to an output root, preserve structure, create dirs.
#'
#' @param input_dir  Character scalar. The source root directory.
#' @param output_dir Character scalar. The destination root directory.
#' @param files      Character vector. File paths under input_dir to be remapped.
#' @return           Character vector of output file paths (same length as files).
#' @export

map_paths <- function(input_dir, output_dir, files) {
  stopifnot(is.character(input_dir), length(input_dir) == 1L)
  stopifnot(is.character(output_dir), length(output_dir) == 1L)
  stopifnot(is.character(files))

  # Normalize to forward slashes to simplify regex/path handling (works on Windows too)
  to_fs <- function(x) chartr("\\", "/", x)
  input_dir <- to_fs(input_dir)
  output_dir <- to_fs(output_dir)
  files_fs <- to_fs(files)

  # Remove any trailing slashes on roots for consistent matching
  trim_trailing_slash <- function(x) sub("/+$", "", x)
  input_dir <- trim_trailing_slash(input_dir)
  output_dir <- trim_trailing_slash(output_dir)

  # Escape regex metacharacters in input_dir for a safe prefix match
  escape_regex <- function(x) gsub("([][{}()+*^$|\\.?\\\\])", "\\\\\\1", x)

  # Build a regex that matches the input_dir as a path prefix (with optional trailing slash)
  prefix_re <- paste0("^", escape_regex(input_dir), "(/|$)")

  # Verify all files are under input_dir (helps catch mistakes early)
  is_under <- grepl(prefix_re, files_fs)
  if (!all(is_under)) {
    bad <- files[!is_under]
    stop(
      "Some paths are not under input_dir:\n - ",
      paste(bad, collapse = "\n - ")
    )
  }

  # Compute the relative part under input_dir
  rel <- sub(prefix_re, "", files_fs)

  # Compose the destination paths
  out_paths <- ifelse(nzchar(rel), paste0(output_dir, "/", rel), output_dir)

  # Create any missing directories for the output files
  out_dirs <- unique(to_fs(dirname(out_paths)))
  # If a path resolves to "." (possible if out_paths are just filenames), skip it
  out_dirs <- out_dirs[out_dirs != "."]

  if (length(out_dirs)) {
    # Create directories recursively; suppress warnings if they already exist
    invisible(lapply(out_dirs, function(d) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }))
  }

  # Return the remapped file list
  out_paths
}
