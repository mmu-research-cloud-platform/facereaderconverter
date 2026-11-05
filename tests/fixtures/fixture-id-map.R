synthetic_fixture_id_map <- function() {
  withr::with_preserve_seed({
    RNGkind(
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    set.seed(20261104)
    ids <- 1000:9999
    permutation <- sample(ids, length(ids))
    mapped <- c(permutation[-1L], permutation[[1L]])
    stats::setNames(as.character(mapped), as.character(permutation))
  })
}

map_synthetic_fixture_id <- function(id) {
  ids <- synthetic_fixture_id_map()
  value <- as.character(id)
  mapped <- unname(ids[value])
  if (anyNA(mapped)) {
    stop("Synthetic fixture IDs must be four-digit values from 1000 to 9999.")
  }
  mapped
}

unmap_synthetic_fixture_id <- function(id) {
  ids <- synthetic_fixture_id_map()
  reverse <- stats::setNames(names(ids), unname(ids))
  value <- as.character(id)
  original <- unname(reverse[value])
  if (anyNA(original)) {
    stop("Synthetic fixture IDs must be four-digit values from 1000 to 9999.")
  }
  original
}
