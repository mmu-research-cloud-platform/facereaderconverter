detect_fr_header_row <- function(lines) {
  hits <- !is.na(lines) & grepl("^[[:space:]]*Video Time($|[[:space:]])", lines)
  header_row <- which(hits)[1]
  if (is.na(header_row)) {
    stop("FaceReader header row not found")
  }
  header_row
}
