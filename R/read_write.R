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

#' Read a Qualtrics export
#'
#' Reads a CSV, SPSS (.sav) or Excel (.xlsx) file exported from Qualtrics,
#' keeping the extra header rows (question text, ImportId) so the file can be
#' written back unchanged apart from the masked columns.
#'
#' @param path Path to the file.
#' @param header_rows Number of header rows in CSV/Excel files. Detected
#'   automatically when `NULL`.
#' @return A list with `data` (a data frame), `header` (extra header rows, if
#'   any), `names` (original column names) and `format`.
#' @export
read_qualtrics <- function(path, header_rows = NULL) {
  if (!file.exists(path)) stop("File not found: ", path)
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    csv = read_q_csv(path, header_rows),
    sav = list(data = haven::read_sav(path), header = NULL, format = "sav",
               names = NULL),
    xlsx = read_q_xlsx(path, header_rows),
    stop("Unsupported file type '.", ext, "'. Use a .csv, .sav or .xlsx export.")
  )
}

read_q_csv <- function(path, header_rows) {
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

read_q_xlsx <- function(path, header_rows) {
  top <- readxl::read_excel(path, col_names = FALSE, col_types = "text",
                            n_max = 3, .name_repair = "minimal")
  top <- as.data.frame(top, stringsAsFactors = FALSE)
  h <- if (is.null(header_rows)) detect_header_rows(top) else header_rows
  data <- readxl::read_excel(path, col_names = FALSE, skip = h,
                             guess_max = 1e5, .name_repair = "minimal")
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  orig_names <- unlist(top[1, ], use.names = FALSE)
  names(data) <- make.unique(orig_names)
  list(data = data, header = top[seq_len(h), , drop = FALSE], names = orig_names,
       format = "xlsx", sheet = readxl::excel_sheets(path)[1])
}

write_qualtrics <- function(x, path) {
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
    sav = haven::write_sav(x$data, path),
    xlsx = {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, x$sheet)
      openxlsx::writeData(wb, 1, x$header, colNames = FALSE)
      openxlsx::writeData(wb, 1, x$data, startRow = nrow(x$header) + 1,
                          colNames = FALSE, keepNA = FALSE)
      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    }
  )
  invisible(path)
}

# Puts masked values back into a column, keeping the SPSS variable label.
replace_column <- function(old, new) {
  label <- attr(old, "label", exact = TRUE)
  if (!is.null(label)) attr(new, "label") <- label
  new
}
