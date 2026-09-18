is_fr_coding <- function(x) {
  is.list(x) &&
    !is.data.frame(x) &&
    !is.null(x$coding) &&
    !is.null(x$episodes)
}

get_fr_metadata <- function(x) {
  if (!is_fr_coding(x) || !is.list(x$metadata)) {
    return(list())
  }
  x$metadata
}

resolve_fr_metadata <- function(value, x, name, default = value) {
  metadata <- get_fr_metadata(x)
  if (!is.null(metadata[[name]])) {
    return(metadata[[name]])
  }
  if (missing(value) || is.null(value)) {
    return(default)
  }
  value
}

update_fr_metadata <- function(x, metadata = list()) {
  current <- get_fr_metadata(x)
  x$metadata <- utils::modifyList(current, metadata)
  x
}

attach_fr_metadata <- function(x, metadata) {
  if (length(metadata) > 0L) {
    attr(x, "fr_metadata") <- metadata
  }
  x
}

fr_coding <- function(
  coding,
  episodes = NULL,
  deltas = NULL,
  metadata = list()
) {
  structure(
    list(
      episodes = episodes,
      deltas = deltas,
      coding = coding,
      metadata = metadata
    ),
    class = c("fr_coding", "list")
  )
}
