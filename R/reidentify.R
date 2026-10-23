#' Turn masked codes back into the original identifiers
#'
#' Reads a file masked by [mask_qualtrics()] and, using the private key file,
#' replaces every `MSK` code (also inside free text) with the original value.
#' `MSKblank` becomes an empty cell again. Codes made with a different key are
#' left as they are. The masked file is not changed; the result is written to
#' `<name>_reidentified_<date>_<time>.<ext>`.
#'
#' @param file Path to a masked file (.csv, .sav or .xlsx).
#' @param key_file Path to the key file used to mask it
#'   (`<name>_KEY_<date>_<time>.txt`).
#' @param output Path of the re-identified file. Defaults to
#'   `<name>_reidentified_<date>_<time>.<ext>`.
#' @param overwrite Replace an existing output file.
#' @param header_rows Number of header rows in CSV/Excel files (detected
#'   automatically).
#' @return Invisibly, a list with the re-identified `data` and the `output`
#'   path.
#' @examples
#' \dontrun{
#' reidentify("my_study_masked_2026-10-23_14-53.csv",
#'            key_file = "my_study_KEY_2026-10-23_14-53.txt")
#' }
#' @export
reidentify <- function(file, key_file, output = NULL, overwrite = FALSE,
                       header_rows = NULL) {
  key <- read_key_file(key_file)
  x <- read_qualtrics(file, header_rows)
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

  cache <- new.env(parent = emptyenv())
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
    m <- gregexpr("(?<![A-Za-z0-9])MSK[A-Za-z0-9]{10,}(?![A-Za-z0-9])", new[todo], perl = TRUE)
    regmatches(new[todo], m) <- lapply(regmatches(new[todo], m), function(tokens) {
      vapply(tokens, function(tok) {
        val <- cache[[tok]]
        if (is.null(val)) {
          val <- decode_token(tok, key)
          assign(tok, val, envir = cache)
        }
        if (is.na(val)) {
          unknown <<- unknown + 1
          tok
        } else {
          restored <<- restored + 1
          val
        }
      }, character(1), USE.NAMES = FALSE)
    })
    if (!identical(new, v)) {
      data[[col]] <- replace_column(v, new)
      changed <- c(changed, col)
    }
  }
  if (restored == 0 && unknown > 0) {
    stop("None of the codes match this key. Is it the key file used to mask this file?", call. = FALSE)
  }

  x$data <- data
  write_qualtrics(x, out_path)
  cat(sprintf("Restored %d value(s) in %d column(s): %s\n", restored, length(changed),
              paste(changed, collapse = ", ")))
  if (unknown) message("Note: ", unknown, " code(s) were not made with this key and were left unchanged.")
  cat("Re-identified file: ", out_path, "\n", sep = "")
  message("This file contains identifiers again: keep it private.")
  invisible(list(data = data, output = out_path))
}

# A token is "MSK" + optional region (e.g. "IT" or "USCT") + code. The region
# length is unknown, so each possibility is tried; only the right one passes
# the key check.
decode_token <- function(token, key) {
  rest <- substring(token, 4)
  for (n in c(0, 2, 4)) {
    if (n && !grepl(sprintf("^[A-Z]{%d}", n), rest)) next
    val <- decrypt_code(substring(rest, n + 1), key)
    if (!is.na(val)) {
      if (startsWith(val, lat_tag)) return(sub(",.*$", "", substring(val, 3)))
      if (startsWith(val, lon_tag)) return(sub("^[^,]*,", "", substring(val, 3)))
      return(val)
    }
  }
  NA_character_
}
