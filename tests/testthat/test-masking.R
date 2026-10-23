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

test_that("codes have the right form and repeat for equal values", {
  m <- mask_values(c("A1B2C3", "", NA, "A1B2C3", " A1B2C3 ", "XYZ"))
  expect_match(m[c(1, 4, 6)], "^MSK[A-Za-z0-9]{12,}$")
  expect_equal(m[2:3], c("MSKblank", "MSKblank"))
  expect_equal(m[1], m[4])
  expect_equal(m[1], m[5])
  expect_false(m[1] == m[6])
})

test_that("the same key gives the same codes; another key does not", {
  dir <- withr_tempdir()
  k1 <- new_key_file(file.path(dir, "k1.txt"))
  k2 <- new_key_file(file.path(dir, "k2.txt"))
  expect_equal(mask_values(c("x1", "y2"), key_file = k1), mask_values(c("x1", "y2"), key_file = k1))
  expect_false(identical(mask_values("x1", key_file = k1), mask_values("x1", key_file = k2)))
  expect_false(identical(mask_values("x1"), mask_values("x1")))
  expect_error(new_key_file(k1), "already exists")
  expect_false(any(grepl("x1", readLines(k1))))
})

test_that("CSV export keeps header rows and masks consistently across columns", {
  local_mocked_bindings(time_stamp = function() "2026-01-01_10-00")
  dir <- withr_tempdir()
  path <- fake_export(dir)
  res <- mask_survey(path, columns = c("IPAddress", "LocationLatitude", "LocationLongitude",
                                          "PROLIFIC_PID", "participantId"),
                        ip = "full", latlon = "full", scan_text = TRUE, mask_text = TRUE)
  raw <- readLines(res$output)
  expect_equal(raw[1:3], readLines(path)[1:3])
  d <- read_qualtrics(res$output)$data
  expect_equal(d$PROLIFIC_PID[1], d$participantId[1])
  expect_equal(d$PROLIFIC_PID[1], d$PROLIFIC_PID[3])
  expect_equal(d$participantId[2], "MSKblank")
  expect_equal(d$LocationLatitude[1], d$LocationLatitude[3])
  expect_false(d$LocationLatitude[1] == d$LocationLongitude[1])
  expect_equal(d$comment[1], paste("my id is", d$PROLIFIC_PID[1]))
  out <- paste(readLines(res$output), collapse = "\n")
  expect_false(grepl("5f1a2b3c4d5e6f7a8b9c0d1e|1\\.2\\.3\\.4|41\\.3083", out))
  report <- readLines(res$report)
  expect_true(any(grepl(digest::digest(file = res$output, algo = "sha256"), report)))
  expect_error(mask_survey(path, columns = "IPAddress", ip = "full"), "already exists")
})

test_that("country_state keeps country and US state", {
  skip_if_not_installed("maps")
  skip_if_not_installed("countrycode")
  expect_equal(latlon_region(c(41.3083, 41.9028, NA), c(-72.9279, 12.4964, 1)),
               c("USCT", "IT", NA))
  dir <- withr_tempdir()
  path <- fake_export(dir)
  res <- mask_survey(path, columns = c("LocationLatitude", "LocationLongitude"),
                        latlon = "country_state", scan_text = FALSE)
  d <- read_qualtrics(res$output)$data
  expect_match(d$LocationLatitude, "^MSK(USCT|IT)[A-Za-z0-9]{12,}$")
  expect_match(d$LocationLatitude[2], "^MSKIT")
})

test_that("suggestions find identifier columns by name and content", {
  d <- data.frame(workerId = "A1B2C3D4E5F6G7", note = "hi", other = "x@y.com",
                  RecipientEmail = NA, stringsAsFactors = FALSE)
  s <- suggest_id_columns(d)
  expect_equal(s$column, c("workerId", "other"))
  s <- suggest_id_columns(d, include_empty = TRUE)
  expect_equal(s$column, c("workerId", "other", "RecipientEmail"))
  expect_equal(s$empty, c(FALSE, FALSE, TRUE))
})

test_that("list_columns marks empty identifier columns", {
  dir <- withr_tempdir()
  path <- file.path(dir, "empty.csv")
  writeLines(c("StartDate,IPAddress,Q1", "Start Date,IP Address,Question",
               "2026-01-01,,yes"), path)
  out <- capture.output(cols <- list_columns(path))
  expect_true(any(grepl("\\* +2 IPAddress \\(empty\\)", out)))
  expect_equal(cols$empty, c(FALSE, TRUE, FALSE))
  expect_error(mask_survey(path, columns = "suggested"), "No identifier")
})

test_that("SPSS files keep variable labels", {
  dir <- withr_tempdir()
  path <- file.path(dir, "s.sav")
  d <- data.frame(workerId = c("A1B2C3D4E5F6G7", ""), age = c(30, 40))
  attr(d$workerId, "label") <- "MTurk worker"
  haven::write_sav(d, path)
  res <- mask_survey(path, columns = "workerId", scan_text = FALSE)
  back <- haven::read_sav(res$output)
  expect_equal(attr(back$workerId, "label"), "MTurk worker")
  expect_equal(back$workerId[2], "MSKblank")
  expect_equal(back$age, c(30, 40), ignore_attr = TRUE)
})

test_that("browser versions are not taken for IP addresses", {
  d <- data.frame(MetaInfo_Version = c("145.0.0.0", "143.0.0.0"), ip = c("1.2.3.4", "5.6.7.8"))
  expect_equal(suggest_id_columns(d)$column, "ip")
  expect_equal(nrow(scan_text(d, "MetaInfo_Version")), 0)
})

test_that("columns can be given by number or as \"suggested\"", {
  dir <- withr_tempdir()
  path <- fake_export(dir)
  res <- mask_survey(path, columns = c(2, 5), ip = "full", scan_text = FALSE)
  d <- read_qualtrics(res$output)$data
  expect_match(d$PROLIFIC_PID, "^MSK")
  expect_equal(d$LocationLatitude[1], "41.3083")
  expect_error(mask_survey(path, columns = 99, overwrite = TRUE), "out of range")
  res <- mask_survey(path, columns = "suggested", ip = "full", latlon = "full",
                        scan_text = FALSE, overwrite = TRUE, key_file = res$key_file)
  expect_match(read_qualtrics(res$output)$data$participantId[1], "^MSK")
})

test_that("country of IP addresses comes from a DB-IP file", {
  dir <- withr_tempdir()
  db <- file.path(dir, "dbip.csv")
  writeLines(c("1.2.3.0,1.2.3.255,NA,US,Connecticut,\"New Haven\",41.3,-72.9",
               "5.6.7.0,5.6.7.255,EU,IT,Lazio,Rome,41.9,12.5",
               "2001:db8::,2001:db8::ffff,EU,FR,,,0,0"), db)
  path <- fake_export(dir)
  res <- mask_survey(path, columns = "IPAddress", ip = "country_state",
                        ip_db = db, scan_text = FALSE)
  d <- read_qualtrics(res$output)$data
  expect_match(d$IPAddress, "^MSK(USCT|IT)[A-Za-z0-9]{12,}$")
  expect_match(d$IPAddress[2], "^MSKIT")
  expect_error(mask_survey(path, columns = "IPAddress", ip = "country_state",
                              overwrite = TRUE), "DB-IP")
})

test_that("ip/latlon are checked, and ignored when there is nothing to apply them to", {
  dir <- withr_tempdir()
  path <- fake_export(dir)
  expect_error(mask_survey(path, columns = "IPAddress", ip = "country"), "must be")
  expect_error(mask_survey(path, columns = c("LocationLatitude", "LocationLongitude"),
                              latlon = "everything"), "must be")
  res <- mask_survey(path, columns = "PROLIFIC_PID", ip = "country_state",
                        latlon = "country_state", scan_text = FALSE)
  expect_match(read_qualtrics(res$output)$data$PROLIFIC_PID, "^MSK")
})

test_that("reidentify restores every original value with the key", {
  local_mocked_bindings(time_stamp = function() "2026-01-01_10-00")
  dir <- withr_tempdir()
  path <- fake_export(dir)
  res <- mask_survey(path, columns = c("IPAddress", "LocationLatitude", "LocationLongitude",
                                          "PROLIFIC_PID", "participantId"),
                        ip = "full", latlon = "country_state", mask_text = TRUE)
  expect_match(res$output, "study_masked_2026-01-01_10-00[.]csv$")
  expect_match(res$report, "study_masked_2026-01-01_10-00_report[.]txt$")
  expect_true(file.exists(file.path(dir, "study_KEY_2026-01-01_10-00.txt")))
  back <- reidentify(res$output, key_file = res$key_file)
  expect_equal(readLines(back$output), readLines(path))
  expect_match(back$output, "study_reidentified_2026-01-01_10-00[.]csv$")
})

test_that("an existing key is never overwritten, and can be reused", {
  local_mocked_bindings(time_stamp = function() "2026-01-01_10-00")
  dir <- withr_tempdir()
  path <- fake_export(dir)
  res1 <- mask_survey(path, columns = "PROLIFIC_PID", scan_text = FALSE)
  key_before <- readLines(res1$key_file)
  expect_error(mask_survey(path, columns = "PROLIFIC_PID", overwrite = TRUE), "key file already exists")
  res2 <- mask_survey(path, columns = "PROLIFIC_PID", overwrite = TRUE,
                         key_file = res1$key_file, scan_text = FALSE)
  expect_equal(readLines(res1$key_file), key_before)
  expect_equal(res1$data$PROLIFIC_PID, res2$data$PROLIFIC_PID)
})

test_that("a wrong key cannot reidentify", {
  dir <- withr_tempdir()
  path <- fake_export(dir)
  res <- mask_survey(path, columns = "PROLIFIC_PID", scan_text = FALSE)
  other <- new_key_file(file.path(dir, "other_KEY.txt"))
  expect_error(reidentify(res$output, key_file = other), "None of the codes match")
  expect_error(reidentify(res$output, key_file = path), "does not look like")
})

test_that("each run's files get the time it ran", {
  dir <- withr_tempdir()
  path <- fake_export(dir)
  local_mocked_bindings(time_stamp = function() "2026-01-01_10-00")
  r1 <- mask_survey(path, columns = "PROLIFIC_PID", scan_text = FALSE)
  local_mocked_bindings(time_stamp = function() "2026-01-01_10-05")
  r2 <- mask_survey(path, columns = "PROLIFIC_PID", scan_text = FALSE)
  expect_true(all(file.exists(c(r1$output, r1$report, r1$key_file, r2$output, r2$report, r2$key_file))))
  expect_match(r2$key_file, "study_KEY_2026-01-01_10-05[.]txt$")
  expect_false(identical(r1$data$PROLIFIC_PID, r2$data$PROLIFIC_PID))
})
