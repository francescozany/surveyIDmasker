#' Turn masked codes back into the original identifiers
#'
#' Reads a file masked by [mask_survey()] and, using the private key file,
#' replaces every `MSK` code (also inside free text) with the original value.
#' `MSKblank` becomes an empty cell again. Codes made with a different key are
#' left as they are. The masked file is not changed; the result is written to
#' `<name>_reidentified_<date>_<time>.<ext>`. Several files masked with the
#' same key can be given at once.
#'
#' @param file Path to a masked file (.csv, .sav, .dta or .xlsx), a vector of such
#'   paths, or a folder (all masked files in it).
#' @param key_file Path to the key file used to mask it
#'   (`<name>_KEY_<date>_<time>.txt`).
#' @param output Path of the re-identified file (one file only). Defaults to
#'   `<name>_reidentified_<date>_<time>.<ext>`.
#' @param overwrite Replace an existing output file.
#' @param header_rows Number of header rows in CSV/Excel files (detected
#'   automatically).
#' @param quiet Do not print what was done.
#' @return Invisibly, a list with the re-identified `data` and the `output`
#'   path. With several files, `data` is a list and `output` a vector, both
#'   named by file.
#' @examples
#' # A small made-up Qualtrics export that comes with the package,
#' # copied to a temporary folder
#' dir <- tempfile("survey")
#' dir.create(dir)
#' file.copy(system.file("extdata", "example_survey.csv", package = "surveyIDmasker"), dir)
#' study <- file.path(dir, "example_survey.csv")
#' res <- mask_survey(study, columns = "suggested", ip = "full", latlon = "full")
#'
#' # With the key, the codes become the original identifiers again
#' back <- reidentify(res$output, key_file = res$key_file)
#' back$data$PROLIFIC_PID
#' @export
reidentify <- function(file, key_file, output = NULL, overwrite = FALSE,
                       header_rows = NULL, quiet = FALSE) {
  key <- read_key_file(key_file)
  files <- unlist(lapply(file, function(f) {
    if (!dir.exists(f)) return(f)
    found <- list.files(f, pattern = "_masked_.*\\.(csv|sav|dta|xlsx)$", ignore.case = TRUE, full.names = TRUE)
    if (!length(found)) stop("No masked files in the folder: ", f, call. = FALSE)
    found
  }))
  if (length(files) == 1) return(reidentify_one(files, key, output, overwrite, header_rows, quiet))
  if (!is.null(output)) stop("`output` can only be used with one file.", call. = FALSE)
  res <- list()
  for (f in files) {
    if (!quiet) cat("\n", basename(f), ":\n", sep = "")
    res[[f]] <- reidentify_one(f, key, NULL, overwrite, header_rows, quiet)
  }
  invisible(list(data = lapply(res, `[[`, "data"),
                 output = vapply(res, `[[`, character(1), "output")))
}

reidentify_one <- function(file, key, output, overwrite, header_rows, quiet = FALSE) {
  x <- read_survey(file, header_rows)
  data <- x$data

  # "study_masked_2026-10-23_14-53.csv" -> "study_reidentified_<now>.csv"
  base <- sub("_masked(_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2})?(\\.[^.]+)$", "\\2", file)
  out_path <- output %||% sub("(\\.[^.]+)$", paste0("_reidentified_", time_stamp(), "\\1"), base)
  if (!overwrite && file.exists(out_path)) {
    stop("Output already exists: ", out_path, "\nUse overwrite = TRUE to replace it.")
  }
  if (normalizePath(out_path, mustWork = FALSE) == normalizePath(file)) {
    stop("The output must be a different file from the masked one.")
  }

  token_re <- "(?<![A-Za-z0-9_-])MSK(?:[A-Z]{2}(?:-[A-Z0-9]{1,3})?_)?[A-Za-z0-9]{10,}(?![A-Za-z0-9_-])"
  decoded <- character()   # token -> original value (NA: not made with this key)
  restored <- 0
  unknown <- 0
  changed <- character()
  for (col in names(data)) {
    v <- data[[col]]
    if (!is.character(v)) next
    has_code <- !is.na(v) & grepl("MSK", v, fixed = TRUE)
    if (!any(has_code)) next
    new <- v
    new[has_code & v == "MSKblank"] <- ""
    todo <- has_code & v != "MSKblank"
    m <- gregexpr(token_re, new[todo], perl = TRUE)
    found <- regmatches(new[todo], m)
    tokens <- unlist(found)
    fresh <- setdiff(unique(tokens), names(decoded))
    if (length(fresh)) decoded <- c(decoded, stats::setNames(decode_tokens(fresh, key), fresh))
    vals <- unname(decoded[tokens])
    restored <- restored + sum(!is.na(vals))
    unknown <- unknown + sum(is.na(vals))
    vals[is.na(vals)] <- tokens[is.na(vals)]
    regmatches(new[todo], m) <- unname(split(vals, factor(rep(seq_along(found), lengths(found)),
                                                          levels = seq_along(found))))
    if (!identical(new, v)) {
      data[[col]] <- replace_column(v, new)
      changed <- c(changed, col)
    }
  }
  if (restored == 0 && unknown > 0) {
    stop("None of the codes in ", basename(file), " match this key (created ", key$created,
         "). The report of the masked file says when its key was created.", call. = FALSE)
  }

  x$data <- data
  write_survey(x, out_path)
  if (!quiet) cat(sprintf("Restored %d value(s) in %d column(s): %s\n", restored, length(changed),
              paste(changed, collapse = ", ")))
  if (unknown && !quiet) message("Note: ", unknown, " code(s) were not made with this key and were left unchanged.")
  if (!quiet) {
    cat("Re-identified file: ", out_path, "\n", sep = "")
    message("This file contains identifiers again: keep it private.")
  }
  invisible(list(data = data, output = out_path))
}

# A token is "MSK" + optional region and "_" (e.g. "MSKIT_", "MSKUS-CT_") +
# code. Codes from before 0.3.0 had the region with no "_" ("MSKUSCT..."): for
# those each possible region length is tried, and only the right one passes
# the key check.
decode_token <- function(token, key) decode_tokens(token, key)

decode_tokens <- function(tokens, key) {
  out <- rep(NA_character_, length(tokens))
  sep <- grepl("_", tokens, fixed = TRUE)
  if (any(sep)) out[sep] <- decrypt_codes(sub("^MSK[^_]*_", "", tokens[sep]), key)
  rest <- substring(tokens, 4)
  for (n in c(0, 2, 4)) {
    try <- which(!sep & is.na(out) & (n == 0 | grepl(sprintf("^[A-Z]{%d}", n), rest)))
    if (length(try)) out[try] <- decrypt_codes(substring(rest[try], n + 1), key)
  }
  vapply(out, decode_value, character(1), USE.NAMES = FALSE)
}

# Latitude and longitude codes hold the whole pair: keep the right half.
decode_value <- function(val) {
  if (is.na(val)) return(val)
  if (startsWith(val, lat_tag)) return(sub(",.*$", "", substring(val, 3)))
  if (startsWith(val, lon_tag)) return(sub("^[^,]*,", "", substring(val, 3)))
  val
}
