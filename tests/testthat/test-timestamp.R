# The fixtures hold real timestamps (DigiCert, Sectigo, FreeTSA), so these
# tests run offline. Certificates are checked at the time of the stamp, so
# they keep passing after the services' certificates expire.
fixture <- function(...) test_path("fixtures", ...)
stamped_report <- function() list.files(fixture(), "^example_survey_masked_.*_report\\.txt$", full.names = TRUE)
stamped_file <- function() sub("_report\\.txt$", ".csv", stamped_report())

# Copies the stamped example (masked file and its timestamped report) to a temporary folder.
copy_stamped <- function() {
  dir <- withr_tempdir()
  file.copy(c(stamped_report(), stamped_file()), dir)
  list(dir = dir, report = file.path(dir, basename(stamped_report())),
       file = file.path(dir, basename(stamped_file())))
}

report_token <- function(report) read_timestamp(split_timestamp(report)$token)

# Writes the report back with another timestamp at its end (or none).
replace_token <- function(report, token) {
  writeBin(split_timestamp(report)$content, report)
  if (!is.null(token)) append_timestamp(report, token)
}

test_that("DER lengths are written in short and long form", {
  expect_equal(der(0x04, as.raw(1:3)), as.raw(c(0x04, 3, 1:3)))
  expect_equal(der(0x04, raw(200))[1:3], as.raw(c(0x04, 0x81, 200)))
  expect_equal(der(0x04, raw(300))[1:4], as.raw(c(0x04, 0x82, 0x01, 0x2c)))
  expect_equal(der_read(der(0x30, der(0x04, raw(300))))$value, der(0x04, raw(300)))
})

test_that("timestamps from DigiCert and Sectigo, at the end of the report, are valid", {
  report <- stamped_report()
  lines <- readLines(report)
  expect_equal(sum(lines == ts_begin), 1)
  expect_equal(lines[length(lines)], ts_end)
  ts <- report_token(report)
  expect_identical(ts$imprint, as.raw(openssl::sha256(split_timestamp(report)$content)))
  v <- verify_token(ts)
  expect_true(v$valid)
  expect_match(v$service, "DigiCert")
  expect_equal(report_timestamp(report)$status, "valid")
  v <- verify_token(report_token(fixture("sectigo_report.txt")))
  expect_true(v$valid)
  expect_match(v$service, "Sectigo")
  expect_equal(report_timestamp(fixture("sectigo_report.txt"))$status, "valid")
})

test_that("the timestamp is read back with Windows line endings too", {
  dir <- withr_tempdir()
  token <- split_timestamp(stamped_report())$token
  path <- file.path(dir, "r.txt")
  writeBin(charToRaw("line one\r\nline two\r\n\r\n"), path)
  append_timestamp(path, token)
  raw <- readBin(path, "raw", file.size(path))
  expect_false(any(grepl("[^\r]\n", rawToChar(raw))))
  parts <- split_timestamp(path)
  expect_identical(parts$token, token)
  expect_equal(rawToChar(parts$content), "line one\r\nline two\r\n\r\n")
})

test_that("a timestamp from a service without a trusted certificate is not valid", {
  tsr <- fixture("freetsa.tsr")
  v <- verify_token(read_timestamp(readBin(tsr, "raw", file.size(tsr))))
  expect_false(v$valid)
  expect_match(v$why, "trusted authority")
})

test_that("a damaged or altered timestamp is not valid", {
  x <- copy_stamped()
  token <- split_timestamp(x$report)$token
  ts <- read_timestamp(token)
  # Change one byte of the signature.
  at <- grepRaw(ts$signature, token, fixed = TRUE) + 10
  bad <- token
  bad[at] <- xor(bad[at], as.raw(0xff))
  replace_token(x$report, bad)
  expect_equal(report_timestamp(x$report)$status, "invalid")
  # Change the time inside the signed content.
  bad <- token
  at <- grepRaw(charToRaw(format(ts$time, "%Y%m%d")), token, fixed = TRUE)
  bad[at + 3] <- as.raw(as.integer(bad[at + 3]) + 1)
  replace_token(x$report, bad)
  expect_equal(report_timestamp(x$report)$status, "invalid")
  replace_token(x$report, token[1:100])
  expect_equal(report_timestamp(x$report)$status, "invalid")
  # Letters that are not base64, or no end line.
  lines <- readLines(x$report)
  lines[length(lines) - 3] <- "not a timestamp !!"
  writeLines(lines, x$report)
  expect_equal(report_timestamp(x$report)$status, "invalid")
  writeLines(lines[-length(lines)], x$report)
  res <- report_timestamp(x$report)
  expect_equal(res$status, "invalid")
  expect_match(res$why, "damaged")
})

test_that("verify_report finds edits to the masked file, the report, or a missing timestamp", {
  x <- copy_stamped()
  res <- verify_report(x$file, quiet = TRUE)
  expect_true(res$matches_report)
  expect_equal(res$timestamp, "valid")
  expect_equal(res$masked_columns, "IPAddress, LocationLatitude, LocationLongitude, PROLIFIC_PID")
  expect_equal(verify_report(x$report, quiet = TRUE)$matches_report, TRUE)
  expect_output(print(verify_report(x$file)), "valid. The report has not changed since")

  lines <- readLines(x$file)
  lines[4] <- sub("MSK", "MSX", lines[4])
  writeLines(lines, x$file)
  res <- verify_report(x$file, quiet = TRUE)
  expect_false(res$matches_report)
  expect_equal(res$timestamp, "valid")
  expect_output(print(verify_report(x$file)), "NO, the file was changed")

  original <- readLines(x$report)
  lines <- original
  lines[grepl("^Rows", lines)] <- "Rows: 6"
  writeLines(lines, x$report)
  expect_equal(verify_report(x$file, quiet = TRUE)$timestamp, "changed")
  expect_output(print(verify_report(x$file)), "THE REPORT WAS CHANGED")

  replace_token(x$report, NULL)
  expect_equal(verify_report(x$file, quiet = TRUE)$timestamp, "missing")
  expect_output(print(verify_report(x$file)), "timestamp at its end was removed")
})

test_that("a renamed copy of a report is recognized", {
  x <- copy_stamped()
  copy <- file.path(x$dir, sub("_report\\.txt$", "_report copy.txt", basename(x$report)))
  file.rename(x$report, copy)
  res <- verify_report(copy, quiet = TRUE)
  expect_true(res$matches_report)
  expect_equal(res$timestamp, "valid")
  expect_equal(verify_report(x$file, quiet = TRUE)$report, basename(copy))
  lines <- readLines(copy)
  lines[1] <- "My report"
  writeLines(lines, copy)
  expect_equal(verify_report(copy, quiet = TRUE)$timestamp, "changed")
  other <- file.path(x$dir, "notes.txt")
  writeLines("not a report", other)
  expect_error(verify_report(other), "not a surveyIDmasker report")
})

test_that("a masked file that was only renamed is recognized by its content", {
  x <- copy_stamped()
  expect_false(verify_report(x$file, quiet = TRUE)$renamed)
  new <- file.path(x$dir, "Study 1 data.csv")
  file.rename(x$file, new)
  for (res in list(verify_report(new, quiet = TRUE), verify_report(x$report, quiet = TRUE))) {
    expect_equal(res$file, "Study 1 data.csv")
    expect_equal(res$listed_as, basename(x$file))
    expect_true(res$renamed)
    expect_true(res$matches_report)
    expect_equal(res$timestamp, "valid")
  }
  expect_output(print(verify_report(new)), "yes, only the file name changed")
  # Renamed and changed: nothing connects it to the report anymore.
  lines <- readLines(new)
  lines[4] <- sub("MSK", "MSX", lines[4])
  writeLines(lines, new)
  expect_true(is.na(verify_report(new, quiet = TRUE)$report))
  res <- verify_report(x$report, quiet = TRUE)
  expect_true(is.na(res$matches_report))
  expect_output(print(verify_report(x$report)), "file not found")
})

test_that("a report whose fingerprint line was damaged still lists the file", {
  x <- copy_stamped()
  lines <- readLines(x$report)
  lines <- sub("^SHA-256 of masked file", "SHA-266 of masked file", lines)
  writeLines(lines, x$report)
  res <- verify_report(x$report, quiet = TRUE)
  expect_equal(res$file, basename(x$file))
  expect_true(is.na(res$matches_report))
  expect_equal(res$timestamp, "changed")
  expect_output(print(verify_report(x$report)), "cannot tell")
})

test_that("text added after the timestamp is a change, and its fingerprints are not used", {
  x <- copy_stamped()
  lines <- readLines(x$file)
  lines[4] <- sub("MSK", "MSX", lines[4])
  writeLines(lines, x$file)
  # Someone adds the fingerprint of the edited file after the timestamp.
  cat("\nMasked file:     ", basename(x$file), "\nSHA-256 of masked file: ",
      digest::digest(file = x$file, algo = "sha256"), "\n", sep = "", file = x$report, append = TRUE)
  res <- verify_report(x$file, quiet = TRUE)
  expect_equal(res$timestamp, "changed")
  expect_false(res$matches_report)
})

test_that("verify_report works without a timestamp, and for files without a report", {
  dir <- withr_tempdir()
  study <- fake_export(dir)
  res <- mask_survey(study, columns = "suggested", ip = "full", latlon = "full", quiet = TRUE)
  out <- verify_report(res$output, quiet = TRUE)
  expect_true(out$matches_report)
  expect_equal(out$timestamp, "none")
  expect_false(res$timestamped)
  expect_false(any(grepl("^Timestamp:", readLines(res$report))))
  file.remove(res$report)
  out <- verify_report(res$output, quiet = TRUE)
  expect_true(is.na(out$report))
  expect_true(is.na(out$matches_report))
  expect_match(out$masked_columns, "PROLIFIC_PID")
})

test_that("verify_report checks every file of a batch report", {
  dir <- withr_tempdir()
  study <- fake_export(dir)
  file.copy(study, file.path(dir, "study2.csv"))
  res <- mask_survey(c(study, file.path(dir, "study2.csv")), columns = "suggested",
                     ip = "full", latlon = "full", quiet = TRUE)
  out <- verify_report(res$report, quiet = TRUE)
  expect_equal(nrow(out), 2)
  expect_true(all(out$matches_report))
  expect_true(verify_report(res$output[[2]], quiet = TRUE)$matches_report)
})

test_that("without an answer from the services, masking still works and the report says so", {
  local_mocked_bindings(request_timestamp = function(url, request) NULL)
  dir <- withr_tempdir()
  res <- mask_survey(fake_export(dir), columns = "suggested", ip = "full", latlon = "full",
                     timestamp = TRUE, quiet = TRUE)
  expect_false(res$timestamped)
  expect_true(file.exists(res$output))
  expect_true(any(grepl("^Timestamp: +not obtained", readLines(res$report))))
  expect_null(split_timestamp(res$report)$token)
  expect_equal(verify_report(res$output, quiet = TRUE)$timestamp, "none")
})

test_that("a timestamp of another file is not kept", {
  other <- split_timestamp(stamped_report())$token
  local_mocked_bindings(request_timestamp = function(url, request) other)
  dir <- withr_tempdir()
  res <- mask_survey(fake_export(dir), columns = "suggested", ip = "full", latlon = "full",
                     timestamp = TRUE, quiet = TRUE)
  expect_false(res$timestamped)
  expect_null(split_timestamp(res$report)$token)
})

test_that("the guided mode asks for a timestamp; Enter means yes", {
  answers <- c("maybe", "")
  local_mocked_bindings(ask_line = function(prompt) {
    a <- answers[1]
    answers <<- answers[-1]
    a
  })
  expect_output(expect_true(choose_timestamp()), "Please type y or n")
  answers <- "n"
  expect_output(expect_false(choose_timestamp()), "independent timestamp")
})

test_that("a real timestamp can be obtained and checked", {
  skip_on_cran()
  skip_if_offline("timestamp.sectigo.com")
  dir <- withr_tempdir()
  res <- mask_survey(fake_export(dir), columns = "suggested", ip = "full", latlon = "full",
                     timestamp = TRUE, quiet = TRUE)
  expect_true(res$timestamped)
  expect_equal(verify_report(res$output, quiet = TRUE)$timestamp, "valid")
})
