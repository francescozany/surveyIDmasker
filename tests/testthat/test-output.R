test_that("quiet = TRUE silences every function, and suppressMessages() does too", {
  dir <- withr_tempdir()
  path <- fake_export(dir)
  expect_silent(res <- mask_survey(path, columns = "suggested", quiet = TRUE))
  expect_silent(reidentify(res$output, key_file = res$key_file, quiet = TRUE))
  expect_silent(verify_report(res$output, quiet = TRUE))
  expect_silent(check_survey(res$output, quiet = TRUE))

  # Without quiet, what is done comes as messages, never as printed text.
  dir2 <- withr_tempdir()
  path2 <- fake_export(dir2)
  expect_message(res2 <- mask_survey(path2, columns = "suggested"), "Masked file")
  expect_silent(suppressMessages(mask_survey(path2, columns = "suggested", ip = "full", latlon = "full",
                                             key_file = res2$key_file, overwrite = TRUE)))
  expect_silent(suppressMessages(reidentify(res2$output, key_file = res2$key_file, overwrite = TRUE)))
  # Two files masked together.
  dir3 <- withr_tempdir()
  a <- fake_export(dir3)
  b <- file.path(dir3, "study2.csv")
  file.copy(a, b)
  expect_silent(suppressMessages(mask_survey(c(a, b), columns = "suggested", ip = "full", latlon = "full")))
})

test_that("results are returned, and printed only when shown", {
  dir <- withr_tempdir()
  path <- fake_export(dir)
  res <- mask_survey(path, columns = "suggested", ip = "full", latlon = "full", quiet = TRUE)

  expect_silent(v <- verify_report(res$output))
  expect_s3_class(v, "data.frame")
  expect_true(v$matches_report)
  expect_output(print(v), "Matches the report: yes")
  expect_s3_class(as.data.frame(v), "data.frame")
  # A subset of the rows prints as a plain data frame.
  expect_output(print(v[1, c("file", "matches_report")]), "matches_report")

  expect_silent(found <- check_survey(path))
  expect_output(print(found), "Still looks identifying")
  expect_output(print(found), "PROLIFIC_PID")

  expect_silent(cols <- list_columns(path))
  expect_output(print(cols), "PROLIFIC_PID")
  expect_equal(names(cols), c("number", "column", "suggested", "empty"))
})
