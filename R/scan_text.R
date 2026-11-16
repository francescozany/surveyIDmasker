# Patterns for identifiers typed inside free text.
text_patterns <- c(
  "Prolific ID" = "\\b[0-9a-f]{24}\\b",
  # Not the last part of a random ID such as a file name
  # 1ABC23D4-56E7-8F90-A123-A4BC56789012.jpg, which can look the same.
  "MTurk worker ID" = "(?<![0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-)\\bA(?=[0-9A-Z]*[0-9])[0-9A-Z]{11,20}\\b",
  "email" = "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}",
  "IP address" = "\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b(?<!\\.0\\.0\\.0)",
  # North American numbers written with separators, e.g. (203) 555-0147,
  # +1 203.555.0147, and Chinese mobile numbers (11 digits starting 13-19)
  # that are not part of a longer code such as a Connect ID.
  "phone number" = "(?<![\\w.])(?:\\+?1[ .-]?)?\\(?\\d{3}\\)?[ .-]\\d{3}[ .-]\\d{4}(?![\\w.])|(?<![A-Za-z0-9])(?:\\+?86[ -]?)?1[3-9]\\d{9}(?![A-Za-z0-9])"
)

#' Look for identifiers left in other columns
#'
#' Scans text columns for strings that look like Prolific IDs, MTurk worker
#' IDs, emails, IP addresses or phone numbers, and for any of the `known_ids` (e.g. the
#' values of the columns being masked). Participants sometimes type their ID
#' into open-ended answers.
#'
#' @param data A data frame.
#' @param columns Columns to scan. Defaults to all columns.
#' @param known_ids Exact identifiers to look for.
#' @return A data frame with `column`, `type`, `cells` (number of cells
#'   with a match) and `rows` (the rows of `data` with a match). No matched
#'   values are returned.
#' @examples
#' example <- system.file("extdata", "example_survey.csv", package = "surveyIDmasker")
#' data <- read_survey(example)$data
#' scan_text(data, "comment")
#' @export
scan_text <- function(data, columns = names(data), known_ids = character()) {
  find_text_ids(data, columns, known_ids)
}

# scan_text(), with row numbers counted from `first_row` (e.g. the row of the
# first response in the file, below the header rows).
find_text_ids <- function(data, columns = names(data), known_ids = character(), first_row = 1L) {
  known_ids <- usable_ids(known_ids)
  rows <- list()
  add <- function(col, type, hit) {
    rows[[length(rows) + 1]] <<- data.frame(column = col, type = type, cells = sum(hit),
                                            rows = paste(which(hit) + first_row - 1L, collapse = ", "))
  }
  for (col in columns) {
    x <- data[[col]]
    if (!is.character(x)) next
    x[is.na(x)] <- ""
    hit_any <- rep(FALSE, length(x))
    for (type in names(text_patterns)) {
      hit <- grepl(text_patterns[[type]], x, perl = TRUE)
      hit_any <- hit_any | hit
      if (any(hit)) add(col, type, hit)
    }
    if (length(known_ids)) {
      hit <- lengths(find_known(x, known_ids)) > 0 & !hit_any
      if (any(hit)) add(col, "known ID", hit)
    }
  }
  if (!length(rows)) return(data.frame(column = character(), type = character(),
                                       cells = integer(), rows = character()))
  do.call(rbind, rows)
}

# "row 5" or "rows 5, 9, 12", shortened after `max` rows.
row_text <- function(rows, max = 10) {
  vapply(strsplit(rows, ", ", fixed = TRUE), function(r) {
    shown <- paste(utils::head(r, max), collapse = ", ")
    if (length(r) > max) shown <- paste0(shown, " and ", length(r) - max, " more")
    paste(if (length(r) == 1) "row" else "rows", shown)
  }, character(1))
}

# Replaces identifiers found inside text with their masked codes.
mask_in_text <- function(x, cb, known_ids = character()) {
  x <- as.character(x)
  # regmatches<- turns NA into the text "NA", so empty cells are set aside.
  na <- is.na(x)
  x[na] <- ""
  for (pattern in text_patterns) {
    m <- gregexpr(pattern, x, perl = TRUE)
    regmatches(x, m) <- lapply(regmatches(x, m), function(found) {
      if (length(found)) paste0("MSK", codes_for(cb, found)) else found
    })
  }
  known_ids <- usable_ids(known_ids)
  if (length(known_ids)) {
    found <- find_known(x, known_ids)
    # Longest first, so an ID that contains a shorter one is replaced whole.
    ids <- unique(unlist(found))
    for (id in ids[order(-nchar(ids))]) {
      has <- vapply(found, function(f) id %in% f, logical(1))
      x[has] <- gsub(id, paste0("MSK", codes_for(cb, id)), x[has], fixed = TRUE)
    }
  }
  x[na] <- NA
  x
}

# The known IDs found inside each element of x (a list). Instead of searching
# for every ID in every cell, each distinct cell is cut into all pieces as
# long as an ID, and the pieces are looked up among the IDs at once.
find_known <- function(x, ids) {
  out <- vector("list", length(x))
  x[is.na(x)] <- ""
  u <- unique(x[nchar(x) >= min(nchar(ids))])
  if (!length(u)) return(out)
  hits <- vector("list", length(u))
  by_len <- split(ids, nchar(ids))
  for (chunk in split(seq_along(u), ceiling(seq_along(u) / 500))) {
    cells <- u[chunk]
    for (len in names(by_len)) {
      n <- as.integer(len)
      count <- pmax(nchar(cells) - n + 1, 0)
      if (!sum(count)) next
      cell <- rep(seq_along(cells), count)
      start <- sequence(count)
      piece <- substring(cells[cell], start, start + n - 1)
      ok <- piece %in% by_len[[len]]
      for (i in unique(cell[ok])) {
        hits[[chunk[i]]] <- unique(c(hits[[chunk[i]]], piece[ok & cell == i]))
      }
    }
  }
  idx <- match(x, u)
  out[!is.na(idx)] <- hits[idx[!is.na(idx)]]
  out
}

# Short values, numbers and dates are too common to search for inside text.
usable_ids <- function(ids) {
  unique(ids[!is_blank(ids) & nchar(ids) >= 6 & !grepl("^[-0-9.:/ ]+$", ids)])
}
