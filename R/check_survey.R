#' Check a file for identifiers before sharing it
#'
#' Lists what still looks identifying in a survey file (e.g. a masked file
#' before you share it): identifier columns that are not masked, and
#' identifiers typed into other columns. No key is needed and the file is not
#' changed.
#'
#' @param file Path to a survey data file (.csv, .sav, .dta or .xlsx), a
#'   vector of such paths, or a folder.
#' @param header_rows Number of header rows in CSV/Excel files (detected
#'   automatically).
#' @param quiet Do not print the findings (they are still returned).
#' @return Invisibly, a data frame with one row per finding: `file`,
#'   `column`, `issue` and `cells`.
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
  say <- function(...) if (!quiet) cat(...)
  found <- list()
  for (f in expand_files(file)) {
    data <- read_survey(f, header_rows)$data
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
    say(sprintf("%s: %d rows, %d columns\n", basename(f), nrow(data), ncol(data)))
    if (length(masked)) say("  Masked columns:", paste(masked, collapse = ", "), "\n")
    if (nrow(rows)) {
      say("  Still looks identifying:\n")
      say(sprintf("    %-28s %-36s %d cell(s)\n", rows$column, rows$issue, rows$cells), sep = "")
    } else {
      say("  Nothing else that looks identifying was found.\n")
    }
    if (nrow(rows)) found[[f]] <- cbind(file = basename(f), rows)
  }
  out <- if (length(found)) do.call(rbind, found) else
    data.frame(file = character(), column = character(), issue = character(), cells = numeric())
  rownames(out) <- NULL
  invisible(out)
}

# A column made by mask_survey(): its filled cells are MSK codes.
is_masked_column <- function(x) {
  x <- trimws(as.character(x))
  x <- x[!is_blank(x)]
  length(x) > 0 && all(grepl("^MSK(blank|([A-Z]{2}(-[A-Z0-9]{1,3})?_)?[A-Za-z0-9]{10,})$", x))
}
