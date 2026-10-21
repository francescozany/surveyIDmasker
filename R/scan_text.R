# Patterns for identifiers typed inside free text.
text_patterns <- c(
  "Prolific ID" = "\\b[0-9a-f]{24}\\b",
  "MTurk worker ID" = "\\bA(?=[0-9A-Z]*[0-9])[0-9A-Z]{11,20}\\b",
  "email" = "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}",
  "IP address" = "\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b(?<!\\.0\\.0\\.0)"
)

#' Look for identifiers left in other columns
#'
#' Scans text columns for strings that look like Prolific IDs, MTurk worker
#' IDs, emails or IP addresses, and for any of the `known_ids` (e.g. the
#' values of the columns being masked). Participants sometimes type their ID
#' into open-ended answers.
#'
#' @param data A data frame.
#' @param columns Columns to scan. Defaults to all columns.
#' @param known_ids Exact identifiers to look for.
#' @return A data frame with `column`, `type` and `cells` (number of cells
#'   with a match). No matched values are returned.
#' @export
scan_text <- function(data, columns = names(data), known_ids = character()) {
  known_ids <- usable_ids(known_ids)
  rows <- list()
  for (col in columns) {
    x <- data[[col]]
    if (!is.character(x)) next
    x[is.na(x)] <- ""
    hit_any <- rep(FALSE, length(x))
    for (type in names(text_patterns)) {
      hit <- grepl(text_patterns[[type]], x, perl = TRUE)
      hit_any <- hit_any | hit
      if (any(hit)) rows[[length(rows) + 1]] <- data.frame(column = col, type = type, cells = sum(hit))
    }
    if (length(known_ids)) {
      hit <- Reduce(`|`, lapply(known_ids, grepl, x = x, fixed = TRUE)) & !hit_any
      if (any(hit)) rows[[length(rows) + 1]] <- data.frame(column = col, type = "known ID", cells = sum(hit))
    }
  }
  if (!length(rows)) return(data.frame(column = character(), type = character(), cells = integer()))
  do.call(rbind, rows)
}

# Replaces identifiers found inside text with their masked codes.
mask_in_text <- function(x, cb, known_ids = character()) {
  x <- as.character(x)
  for (pattern in text_patterns) {
    m <- gregexpr(pattern, x, perl = TRUE)
    regmatches(x, m) <- lapply(regmatches(x, m), function(found) {
      if (length(found)) paste0("MSK", codes_for(cb, found)) else found
    })
  }
  known_ids <- usable_ids(known_ids)
  for (id in known_ids) {
    has <- !is.na(x) & grepl(id, x, fixed = TRUE)
    if (any(has)) x[has] <- gsub(id, paste0("MSK", codes_for(cb, id)), x[has], fixed = TRUE)
  }
  x
}

# Short values, numbers and dates are too common to search for inside text.
usable_ids <- function(ids) {
  unique(ids[!is_blank(ids) & nchar(ids) >= 6 & !grepl("^[-0-9.:/ ]+$", ids)])
}
