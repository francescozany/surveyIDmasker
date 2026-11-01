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
  d <- read_survey(res$output)$data
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
  expect_equal(latlon_region(c(41.3083, 41.9028, NA), c(-72.9279, 12.4964, 1)),
               c("US-CT", "IT", NA))
  dir <- withr_tempdir()
  path <- fake_export(dir)
  res <- mask_survey(path, columns = c("LocationLatitude", "LocationLongitude"),
                        latlon = "country_state", scan_text = FALSE)
  d <- read_survey(res$output)$data
  expect_match(d$LocationLatitude, "^MSK(US-CT|IT)_[A-Za-z0-9]{12,}$")
  expect_match(d$LocationLatitude[2], "^MSKIT_")
})

test_that("coastal points just off the map take the nearest country", {
  # Hangzhou, Zhuhai and Weihai fall in the sea on the coarse world map;
  # the middle of the Atlantic has no land nearby.
  expect_true(all(is.na(point_region(c(30.3205, 22.2159, 36.8720), c(120.3834, 113.5501, 121.7546)))))
  expect_equal(latlon_region(c(30.3205, 22.2159, 36.8720, 0), c(120.3834, 113.5501, 121.7546, -30)),
               c("CN", "CN", "CN", NA))
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

test_that("columns named just id are suggested, but not ResponseId", {
  for (nm in c("id", "Id", "ID")) {
    d <- data.frame(x = c("p1", "p2"), ResponseId = c("R_1", "R_2"), q = 1:2)
    names(d)[1] <- nm
    expect_equal(suggest_id_columns(d)$column, nm)
  }
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

test_that("masking inside text leaves empty cells empty", {
  cb <- new_codebook(derive_key(openssl::rand_bytes(32)))
  out <- mask_in_text(c(NA, "ip 1.2.3.4", "fine"), cb)
  expect_true(is.na(out[1]))
  expect_match(out[2], "^ip MSK")
  expect_equal(out[3], "fine")
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
  d <- read_survey(res$output)$data
  expect_match(d$PROLIFIC_PID, "^MSK")
  expect_equal(d$LocationLatitude[1], "41.3083")
  expect_error(mask_survey(path, columns = 99, overwrite = TRUE), "out of range")
  res <- mask_survey(path, columns = "suggested", ip = "full", latlon = "full",
                        scan_text = FALSE, overwrite = TRUE, key_file = res$key_file)
  expect_match(read_survey(res$output)$data$participantId[1], "^MSK")
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
  d <- read_survey(res$output)$data
  expect_match(d$IPAddress, "^MSK(US-CT|IT)_[A-Za-z0-9]{12,}$")
  expect_match(d$IPAddress[2], "^MSKIT_")
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
  expect_match(read_survey(res$output)$data$PROLIFIC_PID, "^MSK")
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
  # The key stays the same; only the history of its use grows.
  key_after <- readLines(res1$key_file)
  expect_equal(key_after[seq_along(key_before)], key_before)
  expect_equal(sum(grepl("^#   .*study\\.csv$", key_after)), 2)
  expect_equal(res1$data$PROLIFIC_PID, res2$data$PROLIFIC_PID)
})

test_that("a wrong key cannot reidentify", {
  dir <- withr_tempdir()
  path <- fake_export(dir)
  res <- mask_survey(path, columns = "PROLIFIC_PID", scan_text = FALSE)
  other <- new_key_file(file.path(dir, "other_KEY.txt"))
  expect_error(reidentify(res$output, key_file = other), "match this key \\(created")
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

# A small Credamo-style SPSS export: Chinese column names, some with
# full-width punctuation, which haven::write_sav() alone refuses.
fake_credamo <- function(dir) {
  d <- data.frame(
    a = c("oB5yZqDnXwd", "mnakKv6dNAO", "Q4Z2rX4b8Bv"),
    b = c("5LwdoXve", "VMJbGkRM", "5LwdoXve"),
    ip = c("120.219.185.45", "183.217.193.60", "120.219.185.45"),
    lon = c("113.8067240", "12.4964", "113.8067240"),
    lat = c("36.0512680", "41.9028", "36.0512680"),
    prov = c("河南省", "江西省", "河南省"),
    city = c("安阳市", "赣州市", "安阳市"),
    q = c(30, 41, 30),
    stringsAsFactors = FALSE
  )
  names(d) <- c("作答ID", "用户ID", "IP", "经度", "纬度",
                "省份", "城市", "您的年龄是？")
  attr(d[[8]], "label") <- "您的年龄是？"
  path <- file.path(dir, "credamo.sav")
  write_sav_any(d, path)
  path
}

test_that("SPSS files with Credamo variable names can be written", {
  dir <- withr_tempdir()
  path <- fake_credamo(dir)
  expect_error(haven::write_sav(haven::read_sav(path), file.path(dir, "x.sav")), "variable names")
  back <- haven::read_sav(path)
  expect_equal(names(back)[8], "您的年龄是？")
  expect_equal(attr(back[[8]], "label"), "您的年龄是？")
  expect_equal(as.numeric(back[[8]]), c(30, 41, 30))
})

test_that("Credamo identifier columns are suggested, but not the response ID", {
  dir <- withr_tempdir()
  d <- haven::read_sav(fake_credamo(dir))
  s <- suggest_id_columns(d)
  expect_equal(s$column, names(d)[2:7])
  expect_true(is_latitude(names(d)[5]) && is_longitude(names(d)[4]))
})

test_that("Credamo exports are masked, coded by country, and reidentified", {
  local_mocked_bindings(time_stamp = function() "2026-01-01_10-00")
  dir <- withr_tempdir()
  path <- fake_credamo(dir)
  res <- mask_survey(path, columns = "suggested", ip = "full", latlon = "country_state")
  d <- haven::read_sav(res$output)
  orig <- haven::read_sav(path)
  expect_equal(names(d), names(orig))
  expect_equal(d[[1]], orig[[1]])
  expect_match(d[[2]], "^MSK[A-Za-z0-9]{32,}$")
  expect_equal(d[[2]][1], d[[2]][3])
  expect_match(d[[5]], "^MSK(CN|IT)_")
  expect_match(d[[5]][2], "^MSKIT_")
  expect_equal(d[[6]][1], d[[6]][3])
  expect_false(d[[6]][1] == d[[6]][2])
  back <- haven::read_sav(reidentify(res$output, key_file = res$key_file)$output)
  expect_equal(lapply(back, as.character), lapply(orig, as.character), ignore_attr = TRUE)
})

test_that("several files are masked with one key and one report", {
  local_mocked_bindings(time_stamp = function() "2026-01-01_10-00")
  dir <- withr_tempdir()
  a <- fake_export(dir)
  b <- file.path(dir, "lab.csv")
  writeLines(c("id,PROLIFIC_PID,score", "p1,5f1a2b3c4d5e6f7a8b9c0d1e,3",
               "p2,bbbbbbbbbbbbbbbbbbbbbbbb,4"), b)
  c3 <- file.path(dir, "clean.csv")
  writeLines(c("condition,score", "a,1", "b,2"), c3)
  res <- mask_survey(c(a, b, c3), columns = "suggested", ip = "full", latlon = "full")
  expect_length(res$output, 2)
  expect_true(all(file.exists(res$output)))
  expect_equal(basename(res$key_file), "batch_KEY_2026-01-01_10-00.txt")
  expect_equal(basename(res$report), "batch_masked_2026-01-01_10-00_report.txt")
  expect_false(any(file.exists(file.path(dir, c("study_KEY_2026-01-01_10-00.txt",
                                                "study_masked_2026-01-01_10-00_report.txt")))))
  # The same Prolific ID has the same code in both files.
  d1 <- read_survey(res$output[[1]])$data
  d2 <- read_survey(res$output[[2]])$data
  expect_equal(d1$PROLIFIC_PID[1], d2$PROLIFIC_PID[1])
  report <- readLines(res$report)
  expect_true(any(grepl("one key for all 2 files, created", report)))
  for (o in res$output) {
    expect_true(any(grepl(digest::digest(file = o, algo = "sha256"), report)))
  }
  expect_true(any(grepl("clean.csv", report)))
  expect_false(any(grepl("5f1a2b3c4d5e6f7a8b9c0d1e", report)))
  key <- readLines(res$key_file)
  expect_true(any(grepl("study.csv$", key)) && any(grepl("lab.csv$", key)))
  expect_false(any(grepl("5f1a2b3c4d5e6f7a8b9c0d1e", key)))
  # Back with one call
  back <- reidentify(res$output, key_file = res$key_file)
  expect_equal(back$data[[2]]$PROLIFIC_PID[1], "5f1a2b3c4d5e6f7a8b9c0d1e")
  expect_equal(read_survey(back$output[[1]])$data$PROLIFIC_PID,
               read_survey(a)$data$PROLIFIC_PID)
})

test_that("a folder stands for its survey files, without earlier outputs", {
  local_mocked_bindings(time_stamp = function() "2026-01-01_10-00")
  dir <- withr_tempdir()
  fake_export(dir)
  writeLines(c("id,score", "p1,3"), file.path(dir, "lab.csv"))
  writeLines(c("id,score", "MSKx,3"), file.path(dir, "old_masked_2025-01-01_10-00.csv"))
  expect_equal(sort(basename(survey_files(dir))), c("lab.csv", "study.csv"))
  res <- mask_survey(dir, columns = "suggested", ip = "full", latlon = "full")
  expect_length(res$output, 2)
})

test_that("several files take column names, a list per file, but not numbers", {
  dir <- withr_tempdir()
  a <- fake_export(dir)
  b <- file.path(dir, "lab.csv")
  writeLines(c("id,PROLIFIC_PID", "p1,5f1a2b3c4d5e6f7a8b9c0d1e"), b)
  expect_error(mask_survey(c(a, b), columns = 1), "Column numbers differ")
  res <- mask_survey(c(a, b), columns = "PROLIFIC_PID", scan_text = FALSE)
  expect_equal(read_survey(res$output[[2]])$data$id, "p1")
  res <- mask_survey(c(a, b), columns = list(study.csv = "PROLIFIC_PID", lab.csv = "id"),
                     scan_text = FALSE, overwrite = TRUE, key_file = res$key_file)
  expect_match(read_survey(res$output[[2]])$data$id, "^MSK")
  expect_equal(read_survey(res$output[[2]])$data$PROLIFIC_PID, "5f1a2b3c4d5e6f7a8b9c0d1e")
  expect_error(mask_survey(c(a, b), columns = "PROLIFIC_PID", output = "x.csv"), "one file")
})

test_that("the macOS dialog's answer is read as a list of files", {
  expect_equal(parse_mac_choice(c("/a/one.csv", "/a/two.sav", ""), NULL), c("/a/one.csv", "/a/two.sav"))
  cancel <- structure("execution error: User canceled. (-128)", status = 1L)
  expect_error(parse_mac_choice(cancel, 1L), "Stopped")
  expect_null(parse_mac_choice("execution error: no user interaction allowed. (-1713)", 1L))
})

test_that("several files picked in one dialog are all read", {
  dir <- withr_tempdir()
  a <- fake_export(dir)
  b <- file.path(dir, "lab.csv")
  writeLines(c("id,score", "p1,3"), b)
  bad <- file.path(dir, "notes.csv")
  writeLines(character(), bad)
  local_mocked_bindings(pick_files = function() c(a, b, bad, a))
  out <- capture.output(files <- choose_files(NULL))
  expect_equal(basename(names(files)), c("study.csv", "lab.csv"))
  expect_true(any(grepl("Could not read notes.csv", out)))
})

test_that("IP and location rules can differ by file", {
  local_mocked_bindings(time_stamp = function() "2026-01-01_10-00")
  dir <- withr_tempdir()
  a <- fake_export(dir)
  b <- file.path(dir, "study2.csv")
  file.copy(a, b)
  cols <- c("LocationLatitude", "LocationLongitude", "IPAddress", "PROLIFIC_PID")
  res <- mask_survey(c(a, b), columns = cols, scan_text = FALSE, ip = "full",
                     latlon = list(study.csv = "country_state", study2.csv = "full"))
  d1 <- read_survey(res$output[[1]])$data
  d2 <- read_survey(res$output[[2]])$data
  expect_match(d1$LocationLatitude, "^MSK(US-CT|IT)_")
  expect_false(any(grepl("_", d2$LocationLatitude)))
  # Same person, same code, whatever the location rule.
  expect_equal(d1$PROLIFIC_PID, d2$PROLIFIC_PID)
  report <- readLines(res$report)
  expect_true(any(grepl("^Locations: +differ by file", report)))
  expect_true(any(grepl("^IP addresses: +full", report)))
  expect_equal(sum(grepl("^   Locations: ", report)), 2)
  back <- reidentify(res$output, key_file = res$key_file)
  expect_equal(back$data[[2]]$LocationLatitude, read_survey(b)$data$LocationLatitude)
  expect_error(mask_survey(c(a, b), columns = cols, latlon = list(other.csv = "full"),
                           key_file = res$key_file, overwrite = TRUE), "not being masked")
  expect_error(mask_survey(c(a, b), columns = cols, latlon = list(study.csv = "partial"),
                           key_file = res$key_file, overwrite = TRUE), "must be")
  # A file left out of the list is masked fully (with a message).
  expect_message(res3 <- mask_survey(c(a, b), columns = cols, ip = "full",
                                     latlon = list(study.csv = "country_state"), scan_text = FALSE,
                                     key_file = res$key_file, overwrite = TRUE),
                 "latitude/longitude fully")
  expect_false(any(grepl("_", read_survey(res3$output[[2]])$data$LocationLatitude)))
})

test_that("the region is always the part between MSK and _", {
  cb <- new_codebook(derive_key(openssl::rand_bytes(32)))
  values <- as.character(1:300)
  regions <- rep(c("IT", "US-CT", NA), 100)
  m <- mask_with(cb, values, prefix = region_prefix(regions))
  # Codes never contain "_" or "-": only region codes do.
  plain <- m[is.na(regions)]
  expect_false(any(grepl("[_-]", plain)))
  expect_equal(sub("^MSK([^_]*)_.*$", "\\1", m[!is.na(regions)]), regions[!is.na(regions)])
})

test_that("codes with a region are read back, also inside text and in the old format", {
  key <- derive_key(openssl::rand_bytes(32))
  cb <- new_codebook(key)
  new <- mask_with(cb, "1.2.3.4", prefix = "MSKUS-CT_")
  old <- mask_with(cb, "5.6.7.8", prefix = "MSKUSCT")
  expect_equal(decode_token(new, key), "1.2.3.4")
  expect_equal(decode_token(old, key), "5.6.7.8")
  dir <- withr_tempdir()
  path <- file.path(dir, "m_masked_2026-01-01_10-00.csv")
  writeLines(c("ip,note", paste0(new, ",seen ", new, ".")), path)
  kf <- file.path(dir, "k.txt")
  write_key_file(key$secret, kf)
  back <- reidentify(path, key_file = kf)
  expect_equal(back$data$ip, "1.2.3.4")
  expect_equal(back$data$note, "seen 1.2.3.4.")
})

test_that("location and contact columns are suggested, flags and countries are not", {
  d <- data.frame(zip = "06511", city = "New Haven", region = "CT", areacode = "203",
                  phone = "203-555-0147", mobile = "1", country = "United States",
                  countrycode = "US", stringsAsFactors = FALSE)
  expect_equal(suggest_id_columns(d)$column, c("zip", "city", "region", "areacode", "phone"))
})

test_that("phone numbers are found and masked in free text", {
  d <- data.frame(comment = c("call me at (203) 555-0147", "我的手机 13812345678",
                              "I scored 2035550147 points", "fine"), stringsAsFactors = FALSE)
  found <- scan_text(d)
  expect_equal(found$type, "phone number")
  expect_equal(found$cells, 2)
  cb <- new_codebook(derive_key(openssl::rand_bytes(32)))
  out <- mask_in_text(d$comment, cb)
  expect_match(out[1], "^call me at MSK[A-Za-z0-9]+$")
  expect_false(grepl("13812345678", out[2]))
  expect_equal(out[3:4], d$comment[3:4])
  # Digits inside a longer code (e.g. a Connect ID) are not a phone number.
  expect_equal(nrow(scan_text(data.frame(x = "4821AB12345678901CD9", stringsAsFactors = FALSE))), 0)
})

test_that("CSV files must be UTF-8", {
  dir <- withr_tempdir()
  gbk <- file.path(dir, "gbk.csv")
  con <- file(gbk, encoding = "GBK")
  writeLines(c("id,城市", "p1,安阳市"), con)
  close(con)
  expect_error(read_survey(gbk), "not saved as UTF-8")
  u16 <- file.path(dir, "u16.csv")
  con <- file(u16, encoding = "UTF-16LE")
  writeLines(c("id,city", "p1,Rome"), con)
  close(con)
  expect_error(read_survey(u16), "not saved as UTF-8")
  bom <- file.path(dir, "bom.csv")
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(enc2utf8("id,城市\np1,安阳市\n"))), bom)
  expect_equal(read_survey(bom)$data[[2]], "安阳市")
})

test_that("the report ends with a methods sentence and the reference", {
  local_mocked_bindings(time_stamp = function() "2026-01-01_10-00")
  dir <- withr_tempdir()
  a <- fake_export(dir)
  b <- file.path(dir, "lab.csv")
  writeLines(c("workerId,zip", "A1B2C3D4E5F6G7,06511"), b)
  res <- mask_survey(c(a, b), columns = "suggested", ip = "full", latlon = "country_state",
                     scan_text = FALSE)
  report <- gsub("\\s+", " ", paste(readLines(res$report), collapse = " "))
  expect_match(report, "For the methods section or README")
  for (kind in c("IP addresses", "locations", "Prolific IDs", "MTurk worker IDs", "zip codes")) {
    expect_match(report, kind, fixed = TRUE)
  }
  expect_match(report, "in both data files")
  expect_match(report, "Masked locations keep the country")
  expect_match(report, "doi.org/10.5281/zenodo")
})

test_that("Stata files keep labels and come back with the key", {
  dir <- withr_tempdir()
  path <- file.path(dir, "study.dta")
  d <- data.frame(PROLIFIC_PID = c("5f1a2b3c4d5e6f7a8b9c0d1e", "", "5f1a2b3c4d5e6f7a8b9c0d1e"),
                  q1 = haven::labelled(c(1, 2, 1), c(Yes = 1, No = 2)), stringsAsFactors = FALSE)
  attr(d$PROLIFIC_PID, "label") <- "Prolific ID"
  attr(d$q1, "label") <- "Do you agree?"
  haven::write_dta(d, path)
  expect_true("study.dta" %in% basename(survey_files(dir)))
  res <- mask_survey(path, columns = "suggested", scan_text = FALSE)
  m <- haven::read_dta(res$output)
  expect_match(m$PROLIFIC_PID[1], "^MSK[A-Za-z0-9]{32,}$")
  expect_equal(m$PROLIFIC_PID[2], "MSKblank")
  expect_equal(m$PROLIFIC_PID[1], m$PROLIFIC_PID[3])
  expect_equal(attr(m$PROLIFIC_PID, "label"), "Prolific ID")
  expect_equal(attr(m$q1, "labels"), attr(d$q1, "labels"))
  expect_equal(attr(m$q1, "label"), "Do you agree?")
  back <- haven::read_dta(reidentify(res$output, key_file = res$key_file)$output)
  expect_equal(back$PROLIFIC_PID, d$PROLIFIC_PID, ignore_attr = TRUE)
})

test_that("codes match the test values in CODES.md", {
  key <- derive_key(as.raw(0:31))
  expect_equal(mask_with(new_codebook(key), c("A1B2C3D4E5F6G7", "5f1a2b3c4d5e6f7a8b9c0d1e", "1.2.3.4")),
               c("MSKyfXOdQODgzfNar1ztKlXIxotIGBQXbGkUnY",
                 "MSKEDme9TFVDrLzRv1RI8HmkzQ10wBQUKnn3yLef0TkMUta1",
                 "MSKBDmEGynURZQHc6ap3tiajChhCXT9lalAfmL0"))
})

test_that("check_survey lists what is still identifying, and nothing in a clean file", {
  local_mocked_bindings(time_stamp = function() "2026-01-01_10-00")
  dir <- withr_tempdir()
  path <- fake_export(dir)
  res <- mask_survey(path, columns = c("PROLIFIC_PID", "IPAddress"), scan_text = FALSE)
  out <- capture.output(found <- check_survey(res$output))
  expect_true(any(grepl("Masked columns: IPAddress, PROLIFIC_PID", out)))
  expect_setequal(found$column, c("LocationLatitude", "LocationLongitude", "participantId", "comment"))
  expect_equal(found$issue[found$column == "comment"], "Prolific ID in text")
  res2 <- mask_survey(path, columns = "suggested", ip = "full", latlon = "full", mask_text = TRUE,
                      key_file = res$key_file, overwrite = TRUE)
  out <- capture.output(found <- check_survey(res2$output))
  expect_equal(nrow(found), 0)
  expect_true(any(grepl("Nothing else", out)))
})

test_that("IDs stored as numbers get the same code as the same IDs written as text", {
  dir <- withr_tempdir()
  key <- new_key_file(file.path(dir, "k.txt"))
  writeLines(c("pid,score", "100000,1", "45000000000,2", "25794,3"), file.path(dir, "a.csv"))
  haven::write_sav(data.frame(pid = c(100000, 45000000000, 25794), score = 1:3), file.path(dir, "b.sav"))
  res <- mask_survey(file.path(dir, c("a.csv", "b.sav")), columns = "pid", scan_text = FALSE, key_file = key)
  expect_equal(read_survey(res$output[[1]])$data$pid, haven::read_sav(res$output[[2]])$pid, ignore_attr = TRUE)
  back <- reidentify(res$output[[2]], key_file = key)
  expect_equal(back$data$pid, c("100000", "45000000000", "25794"), ignore_attr = TRUE)
  expect_equal(id_text(c(1e5, 0.1, -3, NA)), c("100000", "0.1", "-3", NA))
})

test_that("an empty file gives a clear error", {
  dir <- withr_tempdir()
  path <- file.path(dir, "empty.csv")
  file.create(path)
  expect_error(read_survey(path), "empty.csv is empty")
})
