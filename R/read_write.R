header_labels <- c("Start Date", "End Date", "Response Type", "Response ID",
                   "Recorded Date", "IP Address")

# Qualtrics CSV/Excel exports have the variable names in row 1, the question
# text in row 2 and (in newer CSVs) ImportId JSON in row 3.
detect_header_rows <- function(raw) {
  cell_rows <- function(i) if (nrow(raw) >= i) unlist(raw[i, ], use.names = FALSE) else character()
  row2 <- cell_rows(2)
  row3 <- cell_rows(3)
  if (any(grepl('^\\{"ImportId"', row3))) return(3L)
  if (any(row2 %in% header_labels)) return(2L)
  1L
}

#' Read a survey data file
#'
#' Reads a CSV, SPSS (.sav), Stata (.dta) or Excel (.xlsx) survey file (e.g. a Qualtrics or
#' Credamo export, or lab data), keeping any extra header rows (in Qualtrics,
#' question text and ImportId) so the file can be written back unchanged apart
#' from the masked columns.
#'
#' @param path Path to the file.
#' @param header_rows Number of header rows in CSV/Excel files. Detected
#'   automatically when `NULL`.
#' @param sheet For Excel files, the sheet to read (name or number). The first
#'   one by default. [mask_survey()] masks every sheet.
#' @return A list with `data` (a data frame), `header` (extra header rows, if
#'   any), `names` (original column names) and `format`.
#' @examples
#' example <- system.file("extdata", "example_survey.csv", package = "surveyIDmasker")
#' x <- read_survey(example)
#' x$data
#' x$header   # the question text and ImportId rows of the Qualtrics export
#' @export
read_survey <- function(path, header_rows = NULL, sheet = 1) {
  if (!file.exists(path)) stop("File not found: ", path)
  if (file.size(path) == 0) stop(basename(path), " is empty.", call. = FALSE)
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    csv = read_q_csv(path, header_rows),
    sav = list(data = haven::read_sav(path), header = NULL, format = "sav",
               names = NULL),
    dta = list(data = haven::read_dta(path), header = NULL, format = "dta",
               names = NULL),
    xlsx = read_q_xlsx(path, header_rows, sheet),
    stop("Unsupported file type '.", ext, "'. Use a .csv, .sav, .dta or .xlsx file.")
  )
}

read_q_csv <- function(path, header_rows) {
  check_utf8(path)
  raw <- readr::read_csv(path, col_names = FALSE, na = character(),
                         col_types = readr::cols(.default = "c"),
                         trim_ws = FALSE, progress = FALSE,
                         name_repair = "minimal")
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  h <- if (is.null(header_rows)) detect_header_rows(raw) else header_rows
  orig_names <- unlist(raw[1, ], use.names = FALSE)
  data <- raw[-seq_len(h), , drop = FALSE]
  names(data) <- make.unique(orig_names)
  rownames(data) <- NULL
  bom <- identical(readBin(path, "raw", 3), as.raw(c(0xef, 0xbb, 0xbf)))
  list(data = data, header = raw[seq_len(h), , drop = FALSE], names = orig_names,
       format = "csv", bom = bom)
}

read_q_xlsx <- function(path, header_rows, sheet = 1) {
  sheets <- readxl::excel_sheets(path)
  name <- if (is.numeric(sheet)) sheets[sheet] else sheet
  if (length(name) != 1 || is.na(name) || !name %in% sheets) {
    stop(basename(path), " has no sheet ", sheet, ". Its sheets: ", paste(sheets, collapse = ", "), call. = FALSE)
  }
  top <- readxl::read_excel(path, sheet = name, col_names = FALSE, col_types = "text",
                            n_max = 3, .name_repair = "minimal")
  top <- as.data.frame(top, stringsAsFactors = FALSE)
  if (!nrow(top)) {
    return(list(data = data.frame(), header = top, names = character(), format = "xlsx", sheet = name))
  }
  h <- if (is.null(header_rows)) detect_header_rows(top) else header_rows
  data <- readxl::read_excel(path, sheet = name, col_names = FALSE, skip = h,
                             guess_max = 1e5, .name_repair = "minimal")
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  orig_names <- unlist(top[1, ], use.names = FALSE)
  # A sheet with fewer data columns than header columns (or none, when it has
  # only header rows): the missing columns are empty.
  for (j in seq_len(length(orig_names) - ncol(data)) + ncol(data)) data[[j]] <- rep(NA, nrow(data))
  if (ncol(data) > length(orig_names)) {
    extra <- (length(orig_names) + 1):ncol(data)
    orig_names[extra] <- NA
    for (j in extra) top[[j]] <- NA_character_
  }
  orig_names[is.na(orig_names)] <- ""
  names(data) <- make.unique(orig_names)
  list(data = data, header = top[seq_len(h), , drop = FALSE], names = orig_names,
       format = "xlsx", sheet = name)
}

# The parts of a file that are masked: each sheet of an Excel file, or the
# whole file. Named by `path`, or "path [sheet]" for Excel files with several
# sheets (sheet names cannot contain "[" or "]").
read_units <- function(path, header_rows = NULL) {
  if (tolower(tools::file_ext(path)) != "xlsx") return(stats::setNames(list(read_survey(path, header_rows)), path))
  sheets <- readxl::excel_sheets(path)
  if (length(sheets) == 1) return(stats::setNames(list(read_survey(path, header_rows)), path))
  units <- lapply(sheets, function(s) read_survey(path, header_rows, sheet = s))
  stats::setNames(units, paste0(path, " [", sheets, "]"))
}

# The file a part belongs to, and how it is called on screen and in reports.
unit_file <- function(unit) sub(" \\[[^][]*\\]$", "", unit)
unit_label <- function(unit) basename(unit)

# "study.xlsx: 3 sheets (Data: 120 rows, 30 columns; ...)" or "study.csv: 120 rows, 30 columns".
units_summary <- function(units) {
  size <- function(x) sprintf("%d rows, %d columns", nrow(x$data), ncol(x$data))
  if (length(units) == 1) return(sprintf("%s: %s", basename(unit_file(names(units))), size(units[[1]])))
  sprintf("%s: %d sheets (%s)", basename(unit_file(names(units)[1])), length(units),
          paste(vapply(units, function(x) paste0(x$sheet, ": ", size(x)), character(1)), collapse = "; "))
}

# CSV files must be UTF-8 (with or without BOM), as Qualtrics and most tools
# write them. Other encodings (e.g. GBK from Excel in China, or UTF-16) would
# turn non-English text into garbage, so they are refused.
check_utf8 <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  if (length(bytes) >= 3 && identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) bytes <- bytes[-(1:3)]
  if (any(bytes == as.raw(0)) || !validUTF8(rawToChar(bytes[bytes != as.raw(0)]))) {
    stop(basename(path), " is not saved as UTF-8 text, the encoding surveyIDmasker reads.\n",
         "Save it again as UTF-8, e.g. in Excel: File > Save As > \"CSV UTF-8 (Comma delimited)\". ",
         "Check afterwards that non-English text (e.g. Chinese) looks right.", call. = FALSE)
  }
  invisible(TRUE)
}

write_survey <- function(x, path) {
  switch(x$format,
    csv = {
      header <- x$header
      names(header) <- names(x$data)
      body <- x$data
      body[] <- lapply(body, function(col) { col <- as.character(col); col[is.na(col)] <- ""; col })
      out <- rbind(header, body)
      if (isTRUE(x$bom)) {
        readr::write_excel_csv(out, path, col_names = FALSE, na = "")
      } else {
        readr::write_csv(out, path, col_names = FALSE, na = "")
      }
    },
    sav = write_sav_any(x$data, path),
    dta = haven::write_dta(x$data, path),
    xlsx = write_workbook(list(x), path)
  )
  invisible(path)
}

# Writes the sheets (results of read_survey()) into one Excel file, in order.
# Only the values are written. The workbook's author is the computer's user
# name (openxlsx's default).
write_workbook <- function(sheets, path) {
  wb <- openxlsx::createWorkbook()
  for (i in seq_along(sheets)) {
    x <- sheets[[i]]
    openxlsx::addWorksheet(wb, x$sheet)
    if (nrow(x$header)) openxlsx::writeData(wb, i, x$header, colNames = FALSE)
    if (ncol(x$data)) {
      openxlsx::writeData(wb, i, x$data, startRow = nrow(x$header) + 1,
                          colNames = FALSE, keepNA = FALSE)
    }
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

# Credamo uses question text as SPSS variable names, often with full-width
# punctuation ("？", "，"). SPSS and readstat accept these names, but
# haven::write_sav() refuses them, so in that case the data are written with
# haven's own writer without the name check. Names and values are unchanged.
write_sav_any <- function(data, path) {
  tryCatch(haven::write_sav(data, path), error = function(e) {
    if (!grepl("valid SPSS variable names", conditionMessage(e), fixed = TRUE)) stop(e)
    adjust_tz <- utils::getFromNamespace("adjust_tz", "haven")
    write_sav_ <- utils::getFromNamespace("write_sav_", "haven")
    write_sav_(adjust_tz(data), normalizePath(path, mustWork = FALSE), compress = "byte")
  })
  invisible(path)
}

# Puts masked values back into a column, keeping the SPSS variable label.
replace_column <- function(old, new) {
  label <- attr(old, "label", exact = TRUE)
  if (!is.null(label)) attr(new, "label") <- label
  new
}
