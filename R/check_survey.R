#' Check a file for identifiers before sharing it
#'
#' Lists what still looks identifying in a survey file (e.g. a masked file
#' before you share it): identifier columns that are not masked, and
#' identifiers typed into other columns. No key is needed and the file is not
#' changed. Every sheet of an Excel file is checked.
#'
#' @param file Path to a survey data file (.csv, .sav, .dta or .xlsx), a
#'   vector of such paths, or a folder.
#' @param header_rows Number of header rows in CSV/Excel files (detected
#'   automatically).
#' @param quiet Return the findings invisibly, so that they are not printed.
#' @return A data frame with one row per finding: `file`, `column`, `issue`
#'   and `cells`, of class `surveyIDmasker_check`. It prints as a summary of
#'   each file (or sheet); `as.data.frame()` gives the plain table.
#' @examples
#' # A small made-up Qualtrics export that comes with the package,
#' # copied to a temporary folder
#' dir <- tempfile("survey")
#' dir.create(dir)
#' file.copy(system.file("extdata", "example_survey.csv", package = "surveyIDmasker"), dir)
#' study <- file.path(dir, "example_survey.csv")
#' check_survey(study)   # before masking: the identifiers are listed
#'
#' res <- mask_survey(study, columns = "suggested", ip = "full", latlon = "full",
#'                    mask_text = TRUE)
#' check_survey(res$output)   # after masking
#' @export
check_survey <- function(file, header_rows = NULL, quiet = FALSE) {
  summary <- character()
  say <- function(...) summary <<- c(summary, paste0(...))
  found <- list()
  for (f in expand_files(file)) {
    units <- read_units(f, header_rows)
    for (u in names(units)) {
      data <- units[[u]]$data
      masked <- names(data)[vapply(data, is_masked_column, logical(1))]
      cols <- suggest_id_columns(data)
      cols <- cols[!cols$column %in% masked, ]
      text <- scan_text(data, setdiff(names(data), c(masked, cols$column)))
      rows <- rbind(
        data.frame(column = cols$column, issue = cols$reason,
                   cells = vapply(cols$column, function(c) sum(!is_blank(as.character(data[[c]]))), numeric(1))),
        data.frame(column = text$column, issue = paste0(text$type, rep(" in text", nrow(text))),
                   cells = text$cells)
      )
      say(sprintf("%s: %d rows, %d columns", unit_label(u), nrow(data), ncol(data)))
      if (length(masked)) say("  Masked columns: ", paste(masked, collapse = ", "))
      if (nrow(rows)) {
        say("  Still looks identifying:")
        say(sprintf("    %-28s %-36s %d cell(s)", rows$column, rows$issue, rows$cells))
      } else {
        say("  Nothing else that looks identifying was found.")
      }
      if (nrow(rows)) found[[u]] <- cbind(file = unit_label(u), rows)
    }
  }
  out <- if (length(found)) do.call(rbind, found) else
    data.frame(file = character(), column = character(), issue = character(), cells = numeric())
  rownames(out) <- NULL
  out <- with_text(out, summary, "surveyIDmasker_check")
  if (quiet) invisible(out) else out
}

# A column made by mask_survey(): its filled cells are MSK codes.
is_masked_column <- function(x) {
  x <- trimws(as.character(x))
  x <- x[!is_blank(x)]
  length(x) > 0 && all(grepl("^MSK(blank|([A-Z]{2}(-[A-Z0-9]{1,3})?_)?[A-Za-z0-9]{10,})$", x))
}
