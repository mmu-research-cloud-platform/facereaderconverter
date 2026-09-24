write_detailed_export <- function(path, video, participant) {
  writeLines(
    c(
      "Video analysis detailed log",
      "",
      "Face Model\tGeneral",
      "Calibration\t-",
      "Start time\t6/4/2026 13:31:06.331",
      paste("Filename", video, sep = "\t"),
      "Frame rate\t30.000000000",
      "",
      "Video Time\tNeutral\tHappy\tParticipant Name",
      paste("00:00:00.000\t0\t0.5", participant, sep = "\t"),
      paste("00:00:00.033\t0\t0.6", participant, sep = "\t")
    ),
    path
  )
}
