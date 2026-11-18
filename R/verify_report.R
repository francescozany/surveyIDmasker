#' Check a masked file against its report
#'
#' Checks that masked files have not changed since their report was written,
#' and, when the report was timestamped (`mask_survey(timestamp = TRUE)`), that
#' the report itself has not changed since it was timestamped. Also lists the
#' columns that are masked in each file. No key and no internet connection are
#' needed.
#'
#' @param file Path to a masked file (its report is looked for in the same
#'   folder) or to a report (`..._report.txt`, also a renamed copy; every
#'   file it lists is looked for next to it). Several paths can be given. A
#'   masked file that was only renamed is recognized by its content.
#' @param header_rows Number of header rows in CSV/Excel files (detected
#'   automatically).
#' @param quiet Do not print the results (they are still returned).
#' @return Invisibly, a data frame with one row per masked file: `report`,
#'   `file`, `listed_as` (the name in the report), `renamed`,
#'   `matches_report`, `timestamp` (`"valid"`, `"changed"`, `"invalid"`,
#'   `"missing"` or `"none"`), `timestamped_at` and `masked_columns` (for
#'   Excel files with several sheets, by sheet: `"Data: workerId; Pilot:
#'   workerId"`).
#' @examples
#' # A small made-up Qualtrics export that comes with the package,
#' # copied to a temporary folder
#' dir <- tempfile("survey")
#' dir.create(dir)
#' file.copy(system.file("extdata", "example_survey.csv", package = "surveyIDmasker"), dir)
#' study <- file.path(dir, "example_survey.csv")
#'
#' res <- mask_survey(study, columns = "suggested", ip = "full", latlon = "full",
#'                    quiet = TRUE)
#' verify_report(res$output)
#'
#' # With a timestamp (needs an internet connection)
#' # res <- mask_survey(study, columns = "suggested", timestamp = TRUE)
#' @export
verify_report <- function(file, header_rows = NULL, quiet = FALSE) {
  say <- function(...) if (!quiet) cat(...)
  # Which files to check, grouped by report: each file's path, named by the
  # name the report lists it under ("" when no report lists it).
  groups <- list()
  add <- function(report, path, listed) {
    groups[[report]] <<- c(groups[[report]], stats::setNames(path, listed))
  }
  for (f in file) {
    if (!file.exists(f)) stop("File not found: ", f, call. = FALSE)
    if (tolower(tools::file_ext(f)) == "txt") {
      if (!is_report(f)) stop(basename(f), " is not a surveyIDmasker report.", call. = FALSE)
      hashes <- report_hashes(f)
      for (listed in names(hashes)) {
        path <- file.path(dirname(f), listed)
        # A renamed file is found by its content.
        if (!file.exists(path)) path <- find_by_hash(dirname(f), hashes[[listed]]) %||% path
        add(f, path, listed)
      }
    } else {
      hit <- find_report(f)
      if (is.null(hit)) {
        say(basename(f), ": no report in its folder lists this file.\n", sep = "")
        add("(none)", f, "")
      } else {
        add(hit$report, f, hit$listed)
      }
    }
  }
  rows <- list()
  for (report in names(groups)) {
    files <- groups[[report]]
    files <- files[!duplicated(paste(files, names(files)))]
    if (report == "(none)") {
      ts <- list(status = NA_character_)
      hashes <- character()
    } else {
      ts <- report_timestamp(report)
      hashes <- report_hashes(report)
      say("Report: ", basename(report), "\n", sep = "")
      say("  Timestamp: ", timestamp_text(ts), "\n", sep = "")
    }
    for (i in seq_along(files)) {
      f <- files[[i]]
      listed <- names(files)[i]
      found <- file.exists(f)
      renamed <- if (nzchar(listed) && found) basename(f) != listed else NA
      matches <- if (found && nzchar(listed) && !is.na(hashes[listed])) {
        identical(digest::digest(file = f, algo = "sha256"), hashes[[listed]])
      } else NA
      # The masked columns, by sheet for Excel files with several sheets.
      masked <- if (found) {
        units <- read_units(f, header_rows)
        by_part <- lapply(units, function(x) names(x$data)[vapply(x$data, is_masked_column, logical(1))])
        if (length(units) > 1) {
          by_part <- by_part[lengths(by_part) > 0]
          paste0(vapply(units[names(by_part)], `[[`, "", "sheet"), ": ",
                 vapply(by_part, paste, "", collapse = ", "))
        } else by_part[[1]]
      } else character()
      if (report != "(none)") {
        say("  ", basename(f), "\n", sep = "")
        say("    Matches the report: ", if (!found) "file not found next to the report" else
          if (is.na(matches)) "cannot tell, the report has no fingerprint for it" else
          if (matches && renamed) paste0("yes, only the file name changed (the report lists it as ", listed, ")") else
          if (matches) "yes" else "NO, the file was changed after it was masked", "\n", sep = "")
      } else {
        say("  Masked columns in ", basename(f), ": ", sep = "")
      }
      if (found) {
        if (report != "(none)") say("    Masked columns:     ")
        say(if (length(masked)) paste(masked, collapse = if (any(grepl(": ", masked, fixed = TRUE))) "; " else ", ")
            else "none", "\n", sep = "")
      }
      rows[[length(rows) + 1]] <- data.frame(
        report = if (report == "(none)") NA_character_ else basename(report),
        file = basename(f), listed_as = if (nzchar(listed)) listed else NA_character_,
        renamed = renamed, matches_report = matches, timestamp = ts$status,
        timestamped_at = ts$time %||% as.POSIXct(NA, tz = "UTC"),
        masked_columns = paste(masked, collapse = if (any(grepl(": ", masked, fixed = TRUE))) "; " else ", "))
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  invisible(out)
}

timestamp_text <- function(ts) {
  when <- if (!is.null(ts$time)) format(ts$time, "%Y-%m-%d %H:%M:%S UTC")
  switch(ts$status,
    valid = paste0("valid. The report has not changed since ", when, "\n             (timestamped by ",
                   ts$service, ")."),
    changed = paste0("THE REPORT WAS CHANGED after it was timestamped on ", when, "."),
    invalid = paste0("NOT VALID: ", ts$why, "."),
    missing = "MISSING. The report says it was timestamped, but the timestamp at its end was removed.",
    none = "none (not requested when masking).")
}

# The SHA-256 of each masked file listed in a report, named by file. Only
# the part above a timestamp is read, as only that part is timestamped.
report_hashes <- function(report) {
  name <- NA_character_
  out <- character()
  text <- rawToChar(split_timestamp(report)$content)
  Encoding(text) <- "UTF-8"
  for (l in strsplit(text, "\r?\n")[[1]]) {
    if (grepl("^Masked file:", l)) {
      name <- trimws(sub("^Masked file:", "", l))
      out[[name]] <- NA_character_
    } else if (grepl("^[0-9]+\\. .* -> ", l)) {
      name <- sub("^.* -> ", "", l)
      out[[name]] <- NA_character_
    } else if (grepl("^\\s*SHA-256 of masked file:", l) && !is.na(name)) {
      out[[name]] <- trimws(sub(".*:", "", l))
    }
  }
  out
}

# The report in the file's folder that lists it (the newest, if several do),
# and the name it is listed under: found by the file's name or, for a renamed
# file, by its content (fingerprint). NULL when no report lists it.
find_report <- function(file) {
  reports <- list.files(dirname(file), "\\.txt$", ignore.case = TRUE, full.names = TRUE)
  reports <- reports[vapply(reports, is_report, logical(1))]
  reports <- reports[order(file.mtime(reports), decreasing = TRUE)]
  hashes <- lapply(reports, report_hashes)
  for (i in seq_along(reports)) {
    if (basename(file) %in% names(hashes[[i]])) return(list(report = reports[i], listed = basename(file)))
  }
  h <- digest::digest(file = file, algo = "sha256")
  for (i in seq_along(reports)) {
    same <- which(hashes[[i]] == h)
    if (length(same)) return(list(report = reports[i], listed = names(hashes[[i]])[same[1]]))
  }
  NULL
}

# The survey file in `dir` with this fingerprint, or NULL.
find_by_hash <- function(dir, hash) {
  if (is.na(hash)) return(NULL)
  for (f in list.files(dir, "\\.(csv|sav|dta|xlsx)$", ignore.case = TRUE, full.names = TRUE)) {
    if (identical(digest::digest(file = f, algo = "sha256"), hash)) return(f)
  }
  NULL
}

# A report written by mask_survey(), recognized by its content (its first
# line, or a masked file's fingerprint), so renamed copies (e.g. "..._report
# copy.txt") and edited reports are recognized too.
is_report <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  length(lines) > 0 && (identical(sub("^\ufeff", "", lines[1]), "surveyIDmasker report") ||
                          any(grepl("^\\s*SHA-256 of masked file:", lines)))
}
