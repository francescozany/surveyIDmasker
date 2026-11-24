test_that("every sheet of an Excel file is masked, with one key", {
  dir <- withr_tempdir()
  wb <- fake_workbook(dir)
  expect_message(res <- mask_survey(wb, columns = "suggested", ip = "full", mask_text = TRUE),
                 "4 sheets")
  expect_length(res$output, 1)
  expect_equal(readxl::excel_sheets(res$output), c("Data", "Pilot", "Notes", "Empty"))
  data <- read_survey(res$output, sheet = "Data")
  pilot <- read_survey(res$output, sheet = "Pilot")$data
  notes <- read_survey(res$output, sheet = "Notes")$data
  expect_equal(nrow(data$header), 2)
  expect_match(data$data$PROLIFIC_PID, "^MSK")
  expect_match(data$data$IPAddress, "^MSK")
  expect_false(any(grepl("aaaaaaaa", data$data$comment)))
  expect_match(pilot$workerId, "^MSK")
  expect_equal(pilot$score, c(3, 5))
  # The same participant gets the same code in both sheets.
  expect_equal(data$data$PROLIFIC_PID[1], pilot$workerId[1])
  expect_equal(notes, read_survey(wb, sheet = "Notes")$data)
  expect_equal(ncol(read_survey(res$output, sheet = "Empty")$data), 0)
  expect_equal(basename(names(res$data)), c("study.xlsx [Data]", "study.xlsx [Pilot]"))

  report <- readLines(res$report)
  expect_true("Sheets: 4 (Data, Pilot, Notes, Empty)" %in% report)
  expect_true('Sheet "Notes": not masked (no columns to mask)' %in% report)
  expect_true('Sheet "Empty": empty' %in% report)
  expect_true("  Masked columns: workerId" %in% report)
  expect_equal(sum(grepl("^SHA-256 of masked file", report)), 1)

  v <- verify_report(res$output, quiet = TRUE)
  expect_true(v$matches_report)
  expect_equal(v$masked_columns, "Data: IPAddress, PROLIFIC_PID; Pilot: workerId")

  back <- reidentify(res$output, res$key_file, quiet = TRUE)
  expect_named(back$data, c("Data", "Pilot", "Notes", "Empty"))
  expect_equal(back$data$Pilot$workerId, c("5f1a2b3c4d5e6f7a8b9c0d1e", "bbbbbbbbbbbbbbbbbbbbbbbb"))
  expect_equal(back$data$Data$comment, read_survey(wb, sheet = "Data")$data$comment)

  found <- check_survey(res$output, quiet = TRUE)
  expect_equal(nrow(found), 0)
  expect_equal(unique(check_survey(wb, quiet = TRUE)$file), c("study.xlsx [Data]", "study.xlsx [Pilot]"))
})

test_that("the repeat command runs when pasted, also with Windows paths", {
  paths <- c("C:\\Users\\me\\study.xlsx [Data]", "C:\\Users\\me\\study.xlsx [Pilot]")
  cols <- stats::setNames(list("PROLIFIC_PID", c("work\\er", 'say "hi"')), paths)
  cmd <- repeat_command(paths, cols, stats::setNames(c(FALSE, FALSE), paths),
                        ip_of = stats::setNames(c("full", "country"), paths), latlon_of = NULL,
                        ip_db = NULL, key_file = "C:\\Users\\me\\key.csv", mask_text = FALSE)
  call <- parse(text = cmd)[[1]]
  expect_equal(eval(call[[2]]), "C:/Users/me/study.xlsx")
  expect_equal(eval(call$key_file), "C:/Users/me/key.csv")
  expect_equal(eval(call$columns)[["study.xlsx [Pilot]"]], c("work\\er", 'say "hi"'))
  expect_equal(eval(call$ip), list("study.xlsx [Data]" = "full", "study.xlsx [Pilot]" = "country"))
  # A file named with backslashes or forward slashes is the same file.
  expect_equal(columns_for(list("C:/Users/me/a.csv" = "id"), "C:\\Users\\me\\a.csv", "C:\\Users\\me\\a.csv"), "id")
})

test_that("columns can be given by sheet, and a sheet left out is not masked", {
  dir <- withr_tempdir()
  wb <- fake_workbook(dir)
  res <- mask_survey(wb, columns = list("study.xlsx [Data]" = "PROLIFIC_PID"), quiet = TRUE)
  expect_match(read_survey(res$output, sheet = "Data")$data$PROLIFIC_PID, "^MSK")
  expect_equal(read_survey(res$output, sheet = "Pilot")$data$workerId[2], "bbbbbbbbbbbbbbbbbbbbbbbb")
  expect_true('Sheet "Pilot": not masked (no columns to mask)' %in% readLines(res$report))
  expect_error(mask_survey(wb, columns = 2, quiet = TRUE), "sheet to sheet")
})

test_that("the repeat command gives the same masking for a workbook", {
  dir <- withr_tempdir()
  wb <- fake_workbook(dir)
  out <- capture_all(res <- mask_survey(wb, columns = list("study.xlsx [Data]" = c("PROLIFIC_PID", "IPAddress"),
                                                              "study.xlsx [Pilot]" = "workerId"), ip = "full"))
  cmd <- paste(out[seq(grep("To repeat", out) + 1, length(out))], collapse = "\n")
  expect_match(cmd, "study.xlsx [Pilot]", fixed = TRUE)
  cmd <- sub("key_file", "overwrite = TRUE, output = file.path(dir, \"again.xlsx\"), quiet = TRUE, key_file", cmd)
  again <- eval(parse(text = cmd))
  for (s in c("Data", "Pilot")) {
    expect_equal(read_survey(again$output, sheet = s)$data, read_survey(res$output, sheet = s)$data)
  }
})

test_that("the guided mode asks for the columns of each sheet", {
  answers <- c("", "", "", "n")   # Data: Enter; Pilot: Enter; Notes: skip; timestamp: no
  local_mocked_bindings(
    is_interactive = function() TRUE,
    ask_line = function(prompt) {
      a <- answers[1]
      answers <<- answers[-1]
      a
    })
  dir <- withr_tempdir()
  out <- capture_all(res <- mask_survey(fake_workbook(dir), ip = "full", mask_text = FALSE))
  expect_length(answers, 0)
  expect_true(any(grepl("Sheet 1 of 4: study.xlsx [Data]", out, fixed = TRUE)))
  expect_true(any(grepl("Sheet 3 of 4: study.xlsx [Notes]", out, fixed = TRUE)))
  # No question for the empty sheet.
  expect_false(any(grepl("Sheet 4 of 4", out, fixed = TRUE)))
  expect_equal(readxl::excel_sheets(res$output), c("Data", "Pilot", "Notes", "Empty"))
})

test_that("a workbook in a batch with other files", {
  dir <- withr_tempdir()
  wb <- fake_workbook(dir)
  csv <- fake_export(dir)
  res <- mask_survey(c(wb, csv), columns = "suggested", ip = "full", latlon = "full", quiet = TRUE)
  expect_length(res$output, 2)
  v <- verify_report(res$report, quiet = TRUE)
  expect_true(all(v$matches_report))
  report <- readLines(res$report)
  expect_true(any(grepl("^1\\. study.xlsx -> study_masked_", report)))
  expect_true("   Sheets: 4 (Data, Pilot, Notes, Empty)" %in% report)
  expect_true(any(grepl("one key for all 2 files", report)))
})
