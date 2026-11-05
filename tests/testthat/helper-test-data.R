select_test_data_fixture <- function(
  configured = Sys.getenv("TEST_DATA"),
  mode = Sys.getenv("FACEREADER_TEST_FIXTURE", "auto"),
  synthetic = testthat::test_path("..", "fixtures", "synthetic")
) {
  if (!mode %in% c("auto", "real", "synthetic")) {
    stop("FACEREADER_TEST_FIXTURE must be auto, real, or synthetic.")
  }
  real_available <- nzchar(configured) &&
    file.exists(file.path(configured, "test_data.RDa"))
  if (mode == "real" && !real_available) {
    stop("Real fixtures requested but TEST_DATA/test_data.RDa is unavailable.")
  }
  if (mode != "synthetic" && real_available) {
    return(normalizePath(configured, winslash = "/", mustWork = TRUE))
  }
  if (!file.exists(file.path(synthetic, "test_data.RDa"))) {
    stop("The bundled synthetic fixture is missing; regenerate it with tools/generate-test-fixtures.R.")
  }
  normalizePath(synthetic, winslash = "/", mustWork = TRUE)
}

fixture_id_map_environment <- new.env(parent = globalenv())
sys.source(
  testthat::test_path("..", "..", "tools", "fixture-id-map.R"),
  envir = fixture_id_map_environment
)

synthetic_fixture_id_map <- function() {
  fixture_id_map_environment$synthetic_fixture_id_map()
}

map_synthetic_fixture_id <- function(id) {
  fixture_id_map_environment$map_synthetic_fixture_id(id)
}

unmap_synthetic_fixture_id <- function(id) {
  fixture_id_map_environment$unmap_synthetic_fixture_id(id)
}

test_fixture_mode <- function() {
  Sys.getenv("FACEREADER_TEST_FIXTURE_KIND", "real")
}

test_fixture_id <- function(id) {
  if (test_fixture_mode() == "synthetic") {
    map_synthetic_fixture_id(id)
  } else {
    as.character(id)
  }
}

test_fixture_paths <- function(role) {
  root <- Sys.getenv("TEST_DATA")
  is_synthetic <- test_fixture_mode() == "synthetic"
  relative <- switch(
    role,
    detailed_csv = "testdata_detailed.csv",
    c2e_detailed_txt = if (is_synthetic) {
      pairs <- rep(c("a", "b", "c"), each = 2L)
      pair_ids <- c(a = "8892", b = "8894", c = "8895")[pairs]
      participants <- rep(c("10", "11"), 3L)
      file.path(
        "c2e-directory",
        pairs,
        rep(c("parent", "teen"), 3L),
        paste0(
          "Participant ", participants, "_",
          test_fixture_id(pair_ids),
          "_Analysis 1_video_20260101_120000_detailed.txt"
        )
      )
    } else {
      file.path(
        "c2e-directory",
        list.files(
          file.path(root, "c2e-directory"),
          pattern = "_detailed\\.txt$",
          recursive = TRUE,
          full.names = FALSE
        )
      )
    },
    brazil_detailed_xlsx = if (is_synthetic) {
      file.path(
        "brazil",
        paste0(
          "Participant 10_", test_fixture_id("8883"),
          "_Participant 10_Analysis 1_video_20260101_120000_detailed.xlsx"
        )
      )
    } else {
      file.path(
        "brazil",
        "Participant 10_Participant 10_Analysis 1_video_20260918_143140_detailed.xlsx"
      )
    },
    c2e_primary_txt = if (is_synthetic) {
      file.path(
        "c2e-directory", "a", "parent",
        paste0(
          "Participant 10_", test_fixture_id("8892"),
          "_Analysis 1_video_20260101_120000_detailed.txt"
        )
      )
    } else {
      file.path(
        "c2e-directory", "mum", "8892",
        "Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt"
      )
    },
    c2e_secondary_txt = if (is_synthetic) {
      file.path(
        "c2e-directory", "b", "teen",
        paste0(
          "Participant 11_", test_fixture_id("8894"),
          "_Analysis 1_video_20260101_120000_detailed.txt"
        )
      )
    } else {
      file.path(
        "c2e-directory", "mum", "8894",
        "Participant 11_8894_Analysis 1_video_20260721_135456_detailed.txt"
      )
    },
    c2e_failure_txt = if (is_synthetic) {
      file.path(
        "c2e-fail",
        paste0(
          "Participant 10_", test_fixture_id("1267"),
          "_Analysis 1_video_20260101_120000_detailed.txt"
        )
      )
    } else {
      files <- list.files(
        file.path(root, "c2e-fail"),
        pattern = "_detailed\\.txt$",
        recursive = TRUE,
        full.names = FALSE
      )
      if (length(files) != 1L) {
        stop("The c2e-fail fixture must contain exactly one detailed TXT export.")
      }
      file.path("c2e-fail", files)
    },
    fr9_detailed_xlsx = if (is_synthetic) {
      file.path(
        "FR9",
        paste0(
          test_fixture_id("8895"),
          " invented FR9 Participant 1_00024 invented_Analysis 2_video_20260101_120000_detailed.xlsx"
        )
      )
    } else {
      file.path(
        "FR9",
        "8895 mum FR9 Participant 1_00024 mum_Analysis 2_video_20260908_135003_detailed.xlsx"
      )
    },
    fr9_state_xlsx = if (is_synthetic) {
      file.path(
        "FR9",
        paste0(
          test_fixture_id("8895"),
          " invented FR9 Participant 1_00024 invented_Analysis 2_video_20260101_120000_state.xlsx"
        )
      )
    } else {
      file.path(
        "FR9",
        "8895 mum FR9_00024 mum_Analysis 2_video_20260908_135003_state.xlsx"
      )
    },
    fr9_16fps_detailed_xlsx = if (is_synthetic) {
      file.path(
        "FR9",
        paste0(test_fixture_id("1218"), "_invented_FR9_16fps_detailed.xlsx")
      )
    } else {
      file.path(
        "FR9",
        "FR9 1218 participant_Analysis 1_video_20260910_110344_detailed.xlsx"
      )
    },
    stop("Unknown test fixture role: ", role, call. = FALSE)
  )

  paths <- file.path(root, relative)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("The selected fixture role is unavailable: ", role, call. = FALSE)
  }
  paths
}

test_fixture_path <- function(role, index = 1L) {
  candidates <- test_fixture_paths(role)
  if (length(index) != 1L || is.na(index) || index < 1L || index > length(candidates)) {
    stop("The selected fixture role is unavailable: ", role, call. = FALSE)
  }
  candidates[[index]]
}

test_fixture_fps_conflict_paths <- function() {
  id <- map_synthetic_fixture_id("8892")
  root <- testthat::test_path("..", "fixtures", "synthetic", "c2e-fps-conflict")
  paths <- file.path(
    root,
    paste0(
      "Participant 10_", id,
      "_Analysis 1_video_20260101_120000_",
      c(30, 25), "fps_detailed.txt"
    )
  )
  if (any(!file.exists(paths))) {
    stop("The synthetic FPS conflict fixtures are missing; regenerate them with tools/generate-test-fixtures.R.")
  }
  paths
}

fixture_expected_primary_media_id <- function() {
  if (test_fixture_mode() == "synthetic") {
    paste0(
      "#", test_fixture_id("8892"),
      " invented card game session synced 10 minutes_1"
    )
  } else {
    "#8892 Sussed card game teen view point synced 10 minutes_1"
  }
}

fixture_expected_secondary_media_id <- function() {
  if (test_fixture_mode() != "synthetic") {
    stop("This expected media ID is specific to the synthetic fixture.")
  }
  paste0(
    "#", test_fixture_id("8894"),
    " invented card game session synced 10 minutes"
  )
}

load_test_data_fixture <- function() {
  fixture <- file.path(Sys.getenv("TEST_DATA"), "test_data.RDa")
  if (!file.exists(fixture)) {
    testthat::skip("The TEST_DATA test fixture is not available.")
  }

  load(fixture, envir = parent.frame())
}
