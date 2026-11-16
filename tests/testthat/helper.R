withr_tempdir <- function() {
  dir <- tempfile("qm")
  dir.create(dir)
  dir
}

fake_export <- function(dir) {
  path <- file.path(dir, "study.csv")
  lines <- c(
    "StartDate,IPAddress,LocationLatitude,LocationLongitude,PROLIFIC_PID,participantId,comment",
    "Start Date,IP Address,Location Latitude,Location Longitude,PROLIFIC_PID,participantId,Any comments?",
    '"{""ImportId"":""startDate""}","{""ImportId"":""ipAddress""}","{""ImportId"":""locationLatitude""}","{""ImportId"":""locationLongitude""}","{""ImportId"":""PROLIFIC_PID""}","{""ImportId"":""participantId""}","{""ImportId"":""QID1_TEXT""}"',
    "2026-01-01 10:00:00,1.2.3.4,41.3083,-72.9279,5f1a2b3c4d5e6f7a8b9c0d1e,5f1a2b3c4d5e6f7a8b9c0d1e,my id is 5f1a2b3c4d5e6f7a8b9c0d1e",
    "2026-01-01 11:00:00,5.6.7.8,41.9028,12.4964,aaaaaaaaaaaaaaaaaaaaaaaa,,nice study",
    "2026-01-01 12:00:00,1.2.3.4,41.3083,-72.9279,5f1a2b3c4d5e6f7a8b9c0d1e,5f1a2b3c4d5e6f7a8b9c0d1e,"
  )
  writeLines(lines, path)
  path
}
