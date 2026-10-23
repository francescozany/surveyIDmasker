#' Mask participant identifiers in a Qualtrics export
#'
#' Replaces identifiers (Prolific/Connect/MTurk IDs, IP addresses, locations,
#' emails, ...) with `"MSK"` plus a code made by encrypting the value with a
#' private key. The same value gets the same code everywhere in the file, so
#' repeat participants can still be spotted. Blank cells become `"MSKblank"`.
#' Without the key the codes cannot be turned back; with it, [reidentify()]
#' restores the original identifiers.
#'
#' Run it with no arguments to be guided step by step. Anything you give as an
#' argument is not asked, so a call with `file`, `columns`, `ip` and `latlon`
#' runs without any questions (handy for scripts). At the end, the one-line
#' command that repeats the same masking is printed.
#'
#' The original file is never changed. Files written:
#' * `<name>_masked.<ext>`: the data to share;
#' * `<name>_masked_report.txt`: what was masked, when, and a SHA-256 checksum
#'   of the masked file;
#' * `<name>_KEY.txt`, next to the original file (only when no `key_file` is
#'   given): the private key. Keep it with the original data and never share
#'   it.
#'
#' @param file Path to a Qualtrics export (.csv, .sav or .xlsx). If `NULL`, a
#'   file chooser opens.
#' @param columns Columns to mask: names (`c("workerId", "IPAddress")`),
#'   numbers (`c(4, 14, 15)`, as shown by [list_columns()]) or `"suggested"`
#'   for the columns found by [suggest_id_columns()]. If `NULL`, you are asked.
#' @param ip How to mask IP addresses: `"full"` (like any other identifier) or
#'   `"country_state"` (keep the country, and the state for the US, e.g.
#'   `"MSKUSCT..."`, `"MSKIT..."`; needs `ip_db`).
#' @param latlon How to mask latitude/longitude: `"full"` or
#'   `"country_state"`.
#' @param ip_db Path to the free DB-IP "IP to City Lite" CSV file
#'   (<https://db-ip.com/db/lite.php>), needed for `ip = "country_state"`.
#' @param key_file Path to an existing key file. Reusing a key gives the same
#'   codes for the same participants across studies. If `NULL`, a new key is
#'   created and saved as `<name>_KEY.txt` next to the original file.
#' @param scan_text Look for identifiers typed into the other columns.
#' @param mask_text Also replace identifiers found in other columns with
#'   their codes.
#' @param output Path of the masked file. Defaults to `<name>_masked.<ext>`.
#' @param overwrite Replace existing output files.
#' @param header_rows Number of header rows in CSV/Excel files (detected
#'   automatically).
#' @return Invisibly, a list with the masked `data` and the paths of the
#'   `output`, the `report` and the `key_file`.
#' @examples
#' \dontrun{
#' mask_qualtrics()   # guided
#' mask_qualtrics("my_study.sav", columns = c(4, 14, 15, 36),
#'                ip = "full", latlon = "country_state")
#' mask_qualtrics("my_study.csv", columns = "suggested", ip = "country_state",
#'                latlon = "country_state", ip_db = "dbip-city-lite.csv")
#' }
#' @export
mask_qualtrics <- function(file = NULL, columns = NULL, ip = NULL, latlon = NULL,
                           ip_db = NULL, key_file = NULL,
                           scan_text = TRUE, mask_text = FALSE,
                           output = NULL, overwrite = FALSE,
                           header_rows = NULL) {
  ask <- interactive()
  if (ask && (is.null(file) || is.null(columns))) {
    cat("Type q at any question (or press Esc) to exit. Nothing is saved until the end.\n\n")
  }
  if (is.null(file)) {
    if (!ask) stop("Please give the path of the file to mask.")
    cat("Choose the Qualtrics export to mask (.csv, .sav or .xlsx)...\n")
    file <- file.choose()
    if (tolower(basename(file)) %in% quit_words && !file.exists(file)) quit_masking()
  }
  x <- read_qualtrics(file, header_rows)
  data <- x$data
  cat(sprintf("Read %s: %d rows, %d columns.\n", basename(file), nrow(data), ncol(data)))

  columns <- if (is.null(columns)) choose_columns(data, ask) else resolve_columns(data, columns)
  if (!length(columns)) stop("No columns selected, nothing to mask.")

  lat_col <- columns[is_latitude(columns)]
  lon_col <- columns[is_longitude(columns)]
  has_pair <- length(lat_col) == 1 && length(lon_col) == 1
  ip_cols <- columns[vapply(columns, is_ip_column, logical(1), data = data)]

  modes <- c("full", "country_state")
  if (has_pair) latlon <- choose_mode(latlon, "latitude/longitude", ask)
  if (length(ip_cols)) ip <- choose_mode(ip, "IP addresses", ask)
  check_mode <- function(value, name) {
    if (is.null(value)) return(NULL)
    if (!is.character(value) || length(value) != 1 || !value %in% modes) {
      stop(name, ' must be "full" or "country_state".', call. = FALSE)
    }
    value
  }
  latlon <- if (has_pair) check_mode(latlon, "latlon")
  ip <- if (length(ip_cols)) check_mode(ip, "ip")
  if (identical(ip, "country_state") && is.null(ip_db)) {
    ip_db <- choose_ip_db(ask)
    if (is.null(ip_db)) ip <- "full"
  }
  if (!is.null(ip_db) && !file.exists(ip_db)) stop("IP database not found: ", ip_db)

  out_path <- output %||% sub("(\\.[^.]+)$", "_masked\\1", file)
  report_path <- sub("\\.[^.]+$", "_report.txt", out_path)
  if (!overwrite && any(file.exists(c(out_path, report_path)))) {
    stop("Output already exists: ", out_path, "\nUse overwrite = TRUE to replace it.")
  }
  if (normalizePath(out_path, mustWork = FALSE) == normalizePath(file)) {
    stop("The output must be a different file from the original.")
  }
  # A key is never overwritten: that would make earlier masked files
  # impossible to re-identify.
  new_key <- is.null(key_file)
  if (new_key) {
    key_file <- sub("\\.[^.]+$", "_KEY.txt", file)
    if (file.exists(key_file)) {
      stop("A key file already exists: ", key_file,
           "\nReuse it with key_file = \"", key_file, "\", or move it elsewhere first.", call. = FALSE)
    }
    key <- derive_key(openssl::rand_bytes(32))
  } else {
    key <- read_key_file(key_file)
  }

  cb <- new_codebook(key)
  notes <- character()
  originals <- unlist(lapply(columns, function(col) as.character(data[[col]])))
  done <- character()

  # Latitude and longitude are coded from the (lat, lon) pair, so the same
  # place gives the same pair of codes.
  if (has_pair) {
    lat <- as.character(data[[lat_col]])
    lon <- as.character(data[[lon_col]])
    pair <- paste(trimws(lat), trimws(lon), sep = ",")
    prefix <- "MSK"
    if (latlon == "country_state") {
      region <- latlon_region(lat, lon)
      prefix <- ifelse(is.na(region), "MSK", paste0("MSK", region))
      notes <- c(notes, unknown_note(region, lat, "latitude/longitude"))
    }
    data[[lat_col]] <- replace_column(data[[lat_col]],
      mask_with(cb, lat, keys = paste0(lat_tag, pair), prefix = prefix))
    data[[lon_col]] <- replace_column(data[[lon_col]],
      mask_with(cb, lon, keys = paste0(lon_tag, pair), prefix = prefix))
    done <- c(lat_col, lon_col)
  }

  if (identical(ip, "country_state")) {
    cat("Reading the IP database (this can take a minute)...\n")
    db <- read_ip_db(ip_db)
    for (col in ip_cols) {
      addr <- as.character(data[[col]])
      reg <- ip_region(addr, db)
      notes <- c(notes, unknown_note(reg, addr, paste0("IP addresses (", col, ")")))
      data[[col]] <- replace_column(data[[col]],
        mask_with(cb, addr, prefix = ifelse(is.na(reg), "MSK", paste0("MSK", reg))))
      done <- c(done, col)
    }
  }

  for (col in setdiff(columns, done)) {
    data[[col]] <- replace_column(data[[col]], mask_with(cb, data[[col]]))
  }

  text_found <- NULL
  if (scan_text) {
    others <- setdiff(names(data), columns)
    text_found <- scan_text(data, others, known_ids = trimws(originals))
    if (nrow(text_found)) {
      if (mask_text) {
        for (col in unique(text_found$column)) {
          data[[col]] <- replace_column(data[[col]], mask_in_text(data[[col]], cb, trimws(originals)))
        }
        notes <- c(notes, "Identifiers found inside other columns were replaced with their codes (see below).")
      } else {
        notes <- c(notes, "Possible identifiers were found inside other columns (see below). Check them, or rerun with mask_text = TRUE.")
      }
    }
  }

  if (x$format == "sav") {
    long <- vapply(data, function(col) nchar(attr(col, "label", exact = TRUE) %||% "", "bytes") > 256, logical(1))
    if (any(long)) notes <- c(notes, sprintf(
      "%d SPSS variable label(s) longer than 256 characters were shortened (an SPSS file limit): %s.",
      sum(long), paste(names(data)[long], collapse = ", ")))
  }

  x$data <- data
  write_qualtrics(x, out_path)
  if (new_key) write_key_file(key$secret, key_file, used_for = basename(file))
  write_report(report_path, file, out_path, nrow(data), columns,
               if (length(ip_cols)) ip, if (has_pair) latlon,
               notes, text_found, mask_text)

  cat("\nMasked", length(columns), "column(s):", paste(columns, collapse = ", "), "\n")
  for (n in notes) message("Note: ", n)
  if (!is.null(text_found) && nrow(text_found)) print(text_found, row.names = FALSE)
  cat("Masked file: ", out_path, "\nReport:      ", report_path, "\n", sep = "")
  cat("Key:         ", key_file, if (new_key) " (new)" else " (reused)", "\n", sep = "")
  if (new_key) {
    message("\nKEEP THE KEY FILE PRIVATE. Store it with the original data and never share it.\n",
            "It is the only way to turn the codes back into the original identifiers.")
  }
  cat("\nTo repeat this masking in one line:\n",
      repeat_command(file, columns, if (length(ip_cols)) ip, if (has_pair) latlon,
                     ip_db, key_file, mask_text), "\n", sep = "")
  invisible(list(data = data, output = out_path, report = report_path, key_file = key_file))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Latitude and longitude codes encrypt the whole (lat, lon) pair, tagged so
# the two columns get different codes and reidentify() knows which half to
# return.
lat_tag <- "\037a"
lon_tag <- "\037o"

choose_columns <- function(data, ask) {
  found <- suggest_id_columns(data, include_empty = TRUE)
  sugg <- found[!found$empty, ]
  empty <- found$column[found$empty]
  if (!ask) {
    if (!nrow(sugg)) stop("No identifier columns with data were found. Pass them with `columns =`.")
    message("Masking the suggested columns: ", paste(sugg$column, collapse = ", "))
    return(sugg$column)
  }
  cols <- names(data)
  if (!nrow(data)) cat("\nThis file has no responses, so there is nothing to mask.\n")
  if (length(empty)) cat("\nRecognized as identifiers but empty (nothing to mask):", paste(empty, collapse = ", "), "\n")
  if (nrow(sugg)) {
    cat("\nThese columns look like identifiers:\n")
    cat(sprintf("  %-28s %s\n", sugg$column, sugg$reason), sep = "")
    ans <- ask_line("\nPress Enter to mask these, type e to change the selection, or q to exit: ")
    if (tolower(trimws(ans)) != "e") return(sugg$column)
  } else {
    cat("\nNo columns with identifiers to mask were found.\n")
  }
  repeat {
    cat("\nAll columns (* = suggested, (empty) = identifier with no data):\n")
    cat(format_columns(column_labels(cols, sugg$column, empty)), sep = "\n")
    ans <- ask_line(if (nrow(sugg)) "\nNumbers of the columns to mask (e.g. 4 14 15 120-123), Enter for the suggested ones, or q to exit: " else "\nNumbers of the columns to mask (e.g. 4 14 15 120-123), or q to exit: ")
    if (trimws(ans) == "") {
      if (nrow(sugg)) return(sugg$column)
      quit_masking()
    }
    idx <- parse_numbers(ans, length(cols))
    if (length(idx)) {
      cat("Selected:", paste(cols[idx], collapse = ", "), "\n")
      if (tolower(trimws(ask_line("Is this right? [Y/n, or q to exit] "))) %in% c("", "y", "yes")) return(cols[idx])
    } else {
      cat(sprintf("Please type column numbers between 1 and %d, e.g. 4 14 15 or 120-123.\n", length(cols)))
    }
  }
}

column_labels <- function(cols, suggested, empty) {
  sprintf("%s%3d %s%s", ifelse(cols %in% c(suggested, empty), "*", " "), seq_along(cols), cols,
          ifelse(cols %in% empty, " (empty)", ""))
}

parse_numbers <- function(text, max) {
  parts <- strsplit(trimws(text), "[ ,;]+")[[1]]
  idx <- unlist(lapply(parts, function(p) {
    r <- suppressWarnings(as.integer(strsplit(p, "-", fixed = TRUE)[[1]]))
    if (length(r) == 2 && !anyNA(r)) seq(r[1], r[2]) else r
  }))
  idx <- idx[!is.na(idx)]
  if (!length(idx) || any(idx < 1 | idx > max)) return(integer())
  unique(idx)
}

format_columns <- function(labels) {
  width <- max(nchar(labels)) + 2
  per_row <- max(1, floor(getOption("width", 80) / width))
  rows <- split(formatC(labels, width = -width), ceiling(seq_along(labels) / per_row))
  vapply(rows, paste, character(1), collapse = "")
}

write_report <- function(path, input, output, n_rows, columns, ip, latlon,
                         notes, text_found, mask_text) {
  mode_label <- function(m) if (identical(m, "country_state")) "country/US state kept" else "full"
  lines <- c(
    "surveyIDmasker report",
    "======================",
    paste("Created:        ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Package version:", as.character(utils::packageVersion("surveyIDmasker"))),
    paste("Original file:  ", basename(input)),
    paste("Masked file:    ", basename(output)),
    paste("Rows:           ", n_rows),
    "",
    "Masked columns:",
    paste0("  ", columns),
    "",
    if (!is.null(ip)) paste("IP addresses:    ", mode_label(ip)),
    if (!is.null(latlon)) paste("Latitude/longitude:", mode_label(latlon)),
    "Codes:           encrypted with a private key kept by the authors; without",
    "                 the key they cannot be turned back into identifiers",
    "Blank values:    MSKblank"
  )
  if (length(notes)) lines <- c(lines, "", "Notes:", paste0("  - ", notes))
  if (!is.null(text_found) && nrow(text_found)) {
    lines <- c(lines, "", sprintf("Possible identifiers in other columns (%s):",
                                  if (mask_text) "masked" else "NOT masked"),
               sprintf("  %-30s %-18s %d cell(s)", text_found$column, text_found$type, text_found$cells))
  }
  lines <- c(lines, "", paste("SHA-256 of masked file:", digest::digest(file = output, algo = "sha256")))
  writeLines(lines, path)
}

#' List the columns of a Qualtrics export with their numbers
#'
#' Use the numbers in `mask_qualtrics(columns = ...)`. Suggested identifier
#' columns are marked with `*`; identifier columns with no data also show
#' `(empty)`.
#'
#' @param file Path to a Qualtrics export.
#' @return Invisibly, a data frame with `number`, `column`, `suggested` and
#'   `empty`.
#' @export
list_columns <- function(file) {
  data <- read_qualtrics(file)$data
  found <- suggest_id_columns(data, include_empty = TRUE)
  empty <- found$column[found$empty]
  out <- data.frame(number = seq_along(data), column = names(data),
                    suggested = names(data) %in% found$column[!found$empty],
                    empty = names(data) %in% empty)
  cat(format_columns(column_labels(out$column, found$column[!found$empty], empty)), sep = "\n")
  invisible(out)
}

resolve_columns <- function(data, columns) {
  if (identical(columns, "suggested")) {
    columns <- suggest_id_columns(data)$column
    if (!length(columns)) stop("No identifier columns were found. Give them with `columns =`.")
    return(columns)
  }
  if (is.numeric(columns)) {
    bad <- columns[columns < 1 | columns > ncol(data) | columns != round(columns)]
    if (length(bad)) stop("Column number(s) out of range (1-", ncol(data), "): ", paste(bad, collapse = ", "))
    return(unique(names(data)[columns]))
  }
  bad <- setdiff(columns, names(data))
  if (length(bad)) stop("Column(s) not found: ", paste(bad, collapse = ", "),
                        "\nSee the column names and numbers with list_columns(\"<file>\").")
  unique(columns)
}

choose_mode <- function(mode, what, ask) {
  if (!is.null(mode)) return(mode)
  if (!ask) {
    message("Masking ", what, " fully (set it with ", if (what == "IP addresses") "ip" else "latlon", " = ).")
    return("full")
  }
  choice <- ask_choice(paste0("How should ", what, " be masked?"),
                       c("Fully masked, like the other identifiers (MSK + code)",
                         "Keep country, and state for the US (e.g. MSKUSCT..., MSKIT...)"))
  if (choice == 2) "country_state" else "full"
}

choose_ip_db <- function(ask) {
  msg <- paste0("Keeping the country of IP addresses needs the free DB-IP \"IP to City Lite\" CSV.\n",
                "Download it from https://db-ip.com/db/lite.php, unzip it, and give its path with ip_db = .")
  if (!ask) stop(msg, call. = FALSE)
  cat("\n", msg, "\n", sep = "")
  choice <- ask_choice("What would you like to do?",
                       c("Choose the DB-IP file now", "Mask IP addresses fully instead"))
  if (choice == 2) return(NULL)
  db <- file.choose()
  if (tolower(basename(db)) %in% quit_words && !file.exists(db)) quit_masking()
  db
}

quit_words <- c("q", "quit", "exit")

quit_masking <- function() stop("Stopped. Nothing was masked or saved.", call. = FALSE)

# readline() that exits when the user types q.
ask_line <- function(prompt) {
  ans <- trimws(readline(prompt))
  if (tolower(ans) %in% quit_words) quit_masking()
  ans
}

ask_choice <- function(title, options) {
  cat("\n", title, "\n\n", sep = "")
  cat(sprintf("%d: %s\n", seq_along(options), options), sep = "")
  repeat {
    ans <- ask_line("\nSelection (or q to exit): ")
    if (ans %in% as.character(seq_along(options))) return(as.integer(ans))
    cat("Please type", paste(seq_along(options), collapse = " or "), "(or q to exit).\n")
  }
}

unknown_note <- function(region, values, what) {
  n <- sum(is.na(region) & !is_blank(values))
  if (!n) return(character())
  sprintf("%s: %d value(s) could not be matched to a country and got a plain MSK code.", what, n)
}

repeat_command <- function(file, columns, ip, latlon, ip_db, key_file, mask_text) {
  q <- function(x) paste0('"', gsub('"', '\\\\"', x), '"')
  args <- c(q(file),
            paste0("columns = c(", paste(q(columns), collapse = ", "), ")"),
            if (!is.null(ip)) paste0("ip = ", q(ip)),
            if (!is.null(latlon)) paste0("latlon = ", q(latlon)),
            if (!is.null(ip_db)) paste0("ip_db = ", q(ip_db)),
            paste0("key_file = ", q(key_file)),
            if (mask_text) "mask_text = TRUE")
  paste0("mask_qualtrics(", paste(args, collapse = ", "), ")")
}
