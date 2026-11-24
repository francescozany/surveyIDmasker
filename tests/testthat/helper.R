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

# A made-up Excel file with several sheets: a Qualtrics-like "Data" sheet,
# a "Pilot" sheet with one participant in common, a "Notes" sheet without
# identifiers and an empty sheet.
fake_workbook <- function(dir) {
  path <- file.path(dir, "study.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data")
  data <- data.frame(
    StartDate = c("Start Date", "2026-01-01 10:00:00", "2026-01-01 11:00:00", "2026-01-01 12:00:00"),
    IPAddress = c("IP Address", "1.2.3.4", "5.6.7.8", "1.2.3.4"),
    PROLIFIC_PID = c("PROLIFIC_PID", "5f1a2b3c4d5e6f7a8b9c0d1e", "aaaaaaaaaaaaaaaaaaaaaaaa", ""),
    comment = c("Any comments?", "nice", "my id is aaaaaaaaaaaaaaaaaaaaaaaa", "fine"),
    stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "Data", data)
  openxlsx::addWorksheet(wb, "Pilot")
  openxlsx::writeData(wb, "Pilot", data.frame(workerId = c("5f1a2b3c4d5e6f7a8b9c0d1e", "bbbbbbbbbbbbbbbbbbbbbbbb"),
                                               score = c(3, 5)))
  openxlsx::addWorksheet(wb, "Notes")
  openxlsx::writeData(wb, "Notes", data.frame(note = c("Pilot run in January", "Data cleaned by RA")))
  openxlsx::addWorksheet(wb, "Empty")
  openxlsx::saveWorkbook(wb, path)
  path
}

# What a call writes to the console, printed text and messages, in order.
capture_all <- function(code) {
  out <- character()
  con <- textConnection("out", "w", local = TRUE)
  sink(con)
  sink(con, type = "message")
  tryCatch(force(code), finally = {
    sink(type = "message")
    sink()
    close(con)
  })
  out
}
