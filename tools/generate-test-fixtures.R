# Run from the repository root with the development package loaded.
# This generator never reads TEST_DATA or any private export.
generate_test_fixtures <- function(destination = file.path("tests", "fixtures", "synthetic")) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  emotions <- c("neutral", "happy", "sad", "angry", "surprised", "scared", "disgusted")
  frames <- 0:899
  signal <- function(offset, emotion, width = 44L) {
    phase <- (frames - offset - 11L * emotion) %% 450L
    amplitude <- ifelse((frames %/% 150L) %% 2L == 0L, 0.68, 0.24)
    0.01 +
      amplitude * pmax(0, pmin(phase / 6, 1, (width - phase) / 6)) +
      0.08 * pmax(0, pmin((phase - 250) / 6, 1, (280 - phase) / 6))
  }
  groups <- expand.grid(id = c(1L, 3L, 6L), subject = c("parent", "teen"), stringsAsFactors = FALSE)
  test_coding_wide <- dplyr::bind_rows(lapply(seq_len(nrow(groups)), function(i) {
    g <- groups[i, ]
    x <- data.frame(id = g$id, subject = g$subject, video_time = frames / 30)
    for (j in seq_along(emotions)) {
      offset <- if (g$subject == "teen") {
        if (g$id == 1L && emotions[j] == "surprised") 120L else 4L
      } else {
        0L
      }
      width <- if (g$id == 1L && g$subject == "parent" && emotions[j] == "surprised") 140L else 44L
      x[[emotions[j]]] <- signal(offset, j, width)
      if (g$id == 3L && g$subject == "parent" && j != 1L) {
        x[[emotions[j]]] <- 0.01
      }
      if (g$subject == "parent") x[[emotions[j]]][480:515] <- NA_real_
      if (g$id == 1L && g$subject == "parent" && emotions[j] == "happy") {
        x[[emotions[j]]][1:70] <- NA_real_
      }
    }
    x
  }))
  test_coding_wide <- tibble::as_tibble(test_coding_wide)
  test_coding_wide$video_time <- hms::as_hms(test_coding_wide$video_time)
  test_coding <- tidyr::pivot_longer(
    test_coding_wide, tidyselect::all_of(emotions),
    names_to = "emotion", values_to = "value"
  )
  reference <- facereaderconverter::convert_to_episodes(
    test_coding, fps = 30L, delta_window = 0.1, T_up = 0.15,
    T_down = 0.05, delta = 1, min_dur_sec = 0.1,
    consecutive_missing = 90L
  )
  test_data_sync2 <- reference
  test_data_sync2$coding <- reference$coding[, c(
    "id", "subject", "video_time", "emotion", "value", "run_id", "status", "in_state"
  ), with = FALSE]
  test_deltas <- suppressWarnings(facereaderconverter::add_delta_column(
    test_coding, delta = 0.1, delta_window = 0.1, fps = 30L
  ))
  test_deltas[, `:=`(
    behavior = "synthetic", duration = 0,
    modifier_1 = "", modifier_2 = "", modifier_3 = "", modifier_4 = "",
    comment = ""
  )]
  data.table::setcolorder(test_deltas, c(
    "id", "subject", "video_time", "behavior", "duration",
    "modifier_1", "modifier_2", "modifier_3", "modifier_4", "comment",
    "emotion", "value", "frame", "delta"
  ))
  save(
    test_coding, test_coding_wide, test_data_sync2, test_deltas,
    file = file.path(destination, "test_data.RDa"), compress = "xz", version = 2
  )
  csv <- as.data.frame(test_coding_wide[test_coding_wide$id == 1L &
    test_coding_wide$subject == "teen", c("video_time", emotions)])
  csv$video_time <- sprintf(
    "00:%02d:%06.3f", frames %/% 1800L, (frames %% 1800L) / 30
  )
  csv$fail_code <- 0L
  utils::write.csv(csv, file.path(destination, "testdata_detailed.csv"),
    row.names = FALSE, na = ""
  )

  write_export <- function(relative, media, state = FALSE, extra = FALSE, fps = 30) {
    path <- file.path(destination, relative)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    metadata <- list(
      c(if (state) "Video analysis state log" else "Video analysis detailed log"),
      "", c("Face Model", "General"), c("Calibration", "-"),
      c("Start time", "1/1/2000 00:00:00.000"),
      c("Filename", media), c("Frame rate", as.character(fps)), ""
    )
    export_frames <- 0:2099
    export <- csv[rep(seq_len(nrow(csv)), length.out = length(export_frames)), ]
    seconds <- export_frames / fps
    export$video_time <- sprintf("00:%02d:%06.3f", as.integer(seconds %/% 60), seconds %% 60)
    if (state) {
      data <- data.frame(`Video Time` = export$video_time,
        `Dominant Expression` = ifelse(export$happy > 0.15, "Happy", "Neutral"),
        check.names = FALSE
      )
    } else {
      data <- export[c("video_time", emotions)]
      names(data) <- c("Video Time", "Neutral", "Happy", "Sad", "Angry",
        "Surprised", "Scared", "Disgusted")
      if (extra) {
        data$Stimulus <- "invented-shape"
        data$`Event Marker` <- "synthetic"
        data$`Speech Rate` <- "not measured"
      }
    }
    if (grepl("[.]xlsx$", path)) {
      wb <- openxlsx::createWorkbook(creator = "Synthetic fixture generator")
      openxlsx::addWorksheet(wb, "Synthetic")
      for (i in seq_along(metadata)) {
        openxlsx::writeData(wb, 1, t(metadata[[i]]), startRow = i, colNames = FALSE)
      }
      openxlsx::writeData(wb, 1, data, startRow = 9)
      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    } else {
      writeLines(vapply(metadata, paste, collapse = "\t", character(1)), path)
      write(paste(names(data), collapse = "\t"), file = path, append = TRUE)
      utils::write.table(data, path, append = TRUE, sep = "\t",
        row.names = FALSE, col.names = FALSE, quote = FALSE, na = ""
      )
    }
  }
  write_export("FR9/synthetic_fr9_detailed.xlsx", "synthetic-fr9.mp4")
  write_export("FR9/synthetic_fr9_state.xlsx", "synthetic-fr9.mp4", state = TRUE)
  write_export("FR9/synthetic_16fps_detailed.xlsx", "synthetic-sixteen-fps.mp4", fps = 16)
  for (type in c("detailed", "state")) {
    for (ext in c("xlsx", "txt")) {
      write_export(paste0("FR10/synthetic_", ext, "_", type, ".", ext),
        "synthetic-fr10.mp4", state = type == "state", extra = TRUE)
    }
  }
  for (pair in c("a", "b", "c")) {
    for (role in c("parent", "teen")) {
      write_export(
        file.path("c2e-directory", pair, role,
          paste0(pair, "_", role, "_detailed.txt")),
        paste0("synthetic-pair-", pair, ".mp4")
      )
    }
  }
  write_export("c2e-fail/synthetic_failure_detailed.txt", "synthetic-failure.mp4")
  write_export("brazil/synthetic_parent_detailed.xlsx", "synthetic-brazil.mp4")
  write_export("brazil/synthetic_teen_detailed.xlsx", "synthetic-brazil.mp4")
  invisible(destination)
}

if (sys.nframe() == 0L) {
  pkgload::load_all(quiet = TRUE, helpers = FALSE)
  generate_test_fixtures()
}
