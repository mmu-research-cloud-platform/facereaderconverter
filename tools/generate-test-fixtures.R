# Run from the repository root with the development package loaded.
# This generator never reads TEST_DATA or any private export.
source(file.path("tools", "fixture-id-map.R"))

generate_test_fixtures <- function(destination = file.path("tests", "fixtures", "synthetic")) {
  ffmpeg <- Sys.which("ffmpeg")
  if (!nzchar(ffmpeg)) {
    stop("FFmpeg is required to generate the artificial Brazil video fixtures.")
  }
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  obsolete <- file.path(destination, c(
    "FR9/synthetic_fr9_detailed.xlsx",
    "FR9/synthetic_fr9_state.xlsx",
    "FR9/synthetic_16fps_detailed.xlsx",
    "FR10/synthetic_txt_detailed.txt",
    "FR10/synthetic_txt_state.txt",
    "FR10/synthetic_xlsx_detailed.xlsx",
    "FR10/synthetic_xlsx_state.xlsx",
    "brazil/synthetic_parent_detailed.xlsx",
    "brazil/synthetic_teen_detailed.xlsx",
    "c2e-directory/a/parent/a_parent_detailed.txt",
    "c2e-directory/a/teen/a_teen_detailed.txt",
    "c2e-directory/b/parent/b_parent_detailed.txt",
    "c2e-directory/b/teen/b_teen_detailed.txt",
    "c2e-directory/c/parent/c_parent_detailed.txt",
    "c2e-directory/c/teen/c_teen_detailed.txt",
    "c2e-fail/synthetic_failure_detailed.txt"
  ))
  unlink(obsolete, force = TRUE)
  ids <- stats::setNames(
    vapply(
      c(8883, 8884, 8892, 8894, 8895, 1218, 1267),
      map_synthetic_fixture_id,
      character(1)
    ),
    c("8883", "8884", "8892", "8894", "8895", "1218", "1267")
  )
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
  fr9_stem <- paste0(
    ids[["8895"]],
    " invented FR9 Participant 1_00024 invented_Analysis 2_video_20260101_120000"
  )
  fr9_media <- paste0("#", ids[["8895"]], "_invented-session.mp4")
  write_export(
    paste0("FR9/", fr9_stem, "_detailed.xlsx"),
    fr9_media
  )
  write_export(
    paste0("FR9/", fr9_stem, "_state.xlsx"),
    fr9_media,
    state = TRUE
  )
  write_export(
    paste0("FR9/", ids[["1218"]], "_invented_FR9_16fps_detailed.xlsx"),
    paste0("#", ids[["1218"]], "_invented-sixteen-fps.mp4"),
    fps = 16
  )
  for (type in c("detailed", "state")) {
    for (ext in c("xlsx", "txt")) {
      write_export(paste0(
        "FR10/", ids[["8895"]], "_invented_", ext, "_", type, ".", ext
      ),
        paste0("#", ids[["8895"]], "_invented-session.mp4"),
        state = type == "state", extra = TRUE)
    }
  }
  for (pair in c("a", "b", "c")) {
    for (role in c("parent", "teen")) {
      pair_id <- c(a = "8892", b = "8894", c = "8895")[[pair]]
      suffix <- if (pair == "a") "_1" else ""
      participant <- if (role == "parent") "10" else "11"
      write_export(
        file.path("c2e-directory", pair, role,
          paste0(
            "Participant ", participant, "_", ids[[pair_id]],
            "_Analysis 1_video_20260101_120000_detailed.txt"
          )),
        paste0(
          "#", ids[[pair_id]],
          " invented card game session synced 10 minutes",
          suffix, ".mp4"
        )
      )
    }
  }
  fps_conflict_id <- ids[["8892"]]
  fps_conflict_stem <- paste0(
    "Participant 10_", fps_conflict_id,
    "_Analysis 1_video_20260101_120000"
  )
  fps_conflict_media <- paste0("#", fps_conflict_id, "_invented-fps-conflict.mp4")
  for (fps in c(30, 25)) {
    write_export(
      file.path(
        "c2e-fps-conflict",
        paste0(fps_conflict_stem, "_", fps, "fps_detailed.txt")
      ),
      fps_conflict_media,
      fps = fps
    )
  }
  write_export(
    paste0(
      "c2e-fail/Participant 10_", ids[["1267"]],
      "_Analysis 1_video_20260101_120000_detailed.txt"
    ),
    paste0("#", ids[["1267"]], "_invented-failure.mp4")
  )
  matched_media <- paste0("#", ids[c("8883", "8884")], "_invented-session.mp4")
  for (i in seq_along(matched_media)) {
    for (suffix in c("", "_teen")) {
      write_export(
        paste0(
          "brazil/Participant 10_", ids[[c("8883", "8884")[[i]]]],
          "_Participant 10_Analysis 1_video_20260101_120000",
          suffix, "_detailed.xlsx"
        ),
        paste0("C:\\Users\\Public\\Downloads\\", matched_media[[i]])
      )
    }
  }
  unmatched_media <- sub("[.]mp4$", " - Copy.mp4", matched_media[[1L]])
  partial_media <- sub("[.]mp4$", " - Copy (2).mp4", matched_media[[1L]])
  write_export(
    file.path(
      "brazil", "partial",
      paste0(
        "Participant 10_", ids[["8883"]],
        "_Participant 10_Analysis 1_video_20260101_120000_partial_detailed.xlsx"
      )
    ),
    paste0("C:\\Users\\Public\\Downloads\\", partial_media)
  )
  for (media in c(matched_media, unmatched_media, partial_media)) {
    path <- file.path(destination, "brazil", media)
    status <- system2(
      ffmpeg,
      c(
        "-hide_banner", "-loglevel", "error", "-y",
        "-f", "lavfi", "-i", shQuote("testsrc2=size=32x32:rate=30:duration=70"),
        "-an", "-c:v", "libx264", "-threads", "1", "-pix_fmt", "yuv420p",
        "-map_metadata", "-1", shQuote(path)
      )
    )
    if (status != 0L || !file.exists(path) || file.info(path)$size == 0) {
      stop("Could not generate the artificial Brazil video fixture: ", media)
    }
  }
  invisible(destination)
}

if (sys.nframe() == 0L) {
  pkgload::load_all(quiet = TRUE, helpers = FALSE)
  generate_test_fixtures()
}
