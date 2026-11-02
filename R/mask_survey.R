#' Mask participant identifiers in survey files
#'
#' Replaces identifiers (Prolific/Connect/MTurk IDs, IP addresses, locations,
#' emails, ...) with `"MSK"` plus a code made by encrypting the value with a
#' private key. The same value gets the same code everywhere, so repeat
#' participants can still be spotted. Blank cells become `"MSKblank"`.
#' Without the key the codes cannot be turned back; with it, [reidentify()]
#' restores the original identifiers.
#'
#' Several files can be masked at once using the same key: give a vector of
#' files or a folder, or, in the guided mode, add more files after the first.
#' The same identifier then gets the same code in every file.
#'
#' Run it with no arguments to be guided step by step. Anything you give as an
#' argument is not asked, so a call with `file`, `columns`, `ip` and `latlon`
#' runs without any questions (handy for scripts). Nothing is written until
#' every question is answered. At the end, the one-line command that repeats
#' the same masking is printed.
#'
#' The original files are never changed. Files written:
#' * `<name>_masked_<date>_<time>.<ext>`, next to each original: the data to
#'   share;
#' * the report: what was masked, when, with which key (identified only by the
#'   time it was created), and a SHA-256 checksum of each masked file. For one
#'   file it is `<name>_masked_<date>_<time>_report.txt`; for several, one
#'   `batch_masked_<date>_<time>_report.txt` in the folder of the first file;
#' * the private key, only when no `key_file` is given:
#'   `<name>_KEY_<date>_<time>.txt` for one file, `batch_KEY_<date>_<time>.txt`
#'   for several. Keep it with the original data and never share it. The key
#'   file also keeps a list of every file masked with it.
#'
#' `<date>_<time>` is when the run started, e.g. `2026-10-23_14-53`.
#'
#' @param file Path to a survey data file (.csv, .sav, .dta or .xlsx), a vector of
#'   such paths, or a folder (all survey files in it). If `NULL`, a file
#'   window opens; on macOS and Windows several files can be selected in it.
#'   `options(surveyIDmasker.multi_select = FALSE)` uses R's one-file window
#'   instead.
#' @param columns Columns to mask: names (`c("workerId", "IPAddress")`),
#'   numbers (`c(4, 14, 15)`, as shown by [list_columns()]; one file only) or
#'   `"suggested"` for the columns found by [suggest_id_columns()] in each
#'   file. With several files, names are masked in every file that has them,
#'   and a list named by file (`list(study1.csv = "suggested", study2.sav =
#'   c("PROLIFIC_PID"))`) gives each file its own columns. If `NULL`, you are
#'   asked.
#' @param ip How to mask IP addresses: `"full"` (like any other identifier) or
#'   `"country_state"` (keep the country, and the state for the US, e.g.
#'   `"MSKUS-CT_..."`, `"MSKIT_..."`; needs `ip_db`). With several files, one
#'   value applies to all; a list named by file (`list(study1.csv = "full",
#'   study2.sav = "country_state")`) sets each file's rule, e.g. when studies
#'   have different IRB rules on what can be shared. Files left out of the
#'   list are masked fully.
#' @param latlon How to mask latitude/longitude: `"full"` or
#'   `"country_state"`, or a list named by file, as for `ip`.
#' @param ip_db Path to the free DB-IP "IP to City Lite" CSV file
#'   (<https://db-ip.com/db/lite.php>), needed for `ip = "country_state"`.
#' @param key_file Path to an existing key file. Reusing a key gives the same
#'   codes for the same participants across studies. If `NULL`, a new key is
#'   created.
#' @param scan_text Look for identifiers typed into the other columns (of all
#'   the files being masked).
#' @param mask_text Also replace identifiers found in other columns with
#'   their codes.
#' @param output Path of the masked file (one file only). Defaults to
#'   `<name>_masked_<date>_<time>.<ext>`.
#' @param overwrite Replace existing output files.
#' @param header_rows Number of header rows in CSV/Excel files (detected
#'   automatically).
#' @param quiet Do not print what was done (questions in the guided mode are
#'   still asked).
#' @return Invisibly, a list with the masked `data` and the paths of the
#'   `output`, the `report` and the `key_file`. With several files, `data` is
#'   a list and `output` a vector, both named by file.
#' @examples
#' # A small made-up Qualtrics export that comes with the package,
#' # copied to a temporary folder
#' dir <- tempfile("survey")
#' dir.create(dir)
#' file.copy(system.file("extdata", "example_survey.csv", package = "surveyIDmasker"), dir)
#' study <- file.path(dir, "example_survey.csv")
#'
#' res <- mask_survey(study, columns = "suggested", ip = "full",
#'                    latlon = "country_state")
#' read_survey(res$output)$data[, c("IPAddress", "LocationLatitude", "PROLIFIC_PID")]
#'
#' # Guided: choose the file(s) and answer a few questions
#' if (interactive()) mask_survey()
#'
#' # Several files with one key and one report, e.g.
#' # mask_survey(c("study1.csv", "study2.sav"), columns = "suggested")
#' @export
mask_survey <- function(file = NULL, columns = NULL, ip = NULL, latlon = NULL,
                           ip_db = NULL, key_file = NULL,
                           scan_text = TRUE, mask_text = FALSE,
                           output = NULL, overwrite = FALSE,
                           header_rows = NULL, quiet = FALSE) {
  ask <- interactive()
  say <- function(...) if (!quiet) cat(...)
  tell <- function(...) if (!quiet) message(...)
  if (ask && (is.null(file) || is.null(columns))) {
    if (is.null(file)) say("You can mask one file, or several files using the same key (select them together).\n")
    say("Type q at any question (or press Esc) to exit. Nothing is saved until the end.\n\n")
  }

  # 1. The files
  if (is.null(file)) {
    if (!ask) stop("Please give the path of the file to mask.")
    files <- choose_files(header_rows)
  } else {
    files <- expand_files(file)
    files <- lapply(stats::setNames(files, files), function(f) {
      x <- read_survey(f, header_rows)
      say(sprintf("Read %s: %d rows, %d columns.\n", basename(f), nrow(x$data), ncol(x$data)))
      x
    })
  }
  paths <- names(files)
  n <- length(files)
  batch <- n > 1
  if (batch && !is.null(output)) stop("`output` can only be used with one file.", call. = FALSE)
  if (batch && is.numeric(columns)) {
    stop("Column numbers differ from file to file: with several files, give column names, ",
         "\"suggested\", or a list named by file.", call. = FALSE)
  }
  if (batch) say(sprintf("\n%d files will be masked using the same key.\n", n))

  # 2. The columns of each file
  skipped <- character()
  cols <- list()
  for (i in seq_len(n)) {
    data <- files[[i]]$data
    if (batch && is.null(columns) && ask) {
      say("\n", rule(sprintf("File %d of %d: %s", i, n, basename(paths[i]))), "\n", sep = "")
    }
    sel <- if (is.null(columns)) {
      choose_columns(data, ask, batch)
    } else {
      batch_columns(data, columns_for(columns, paths[i], paths), batch, basename(paths[i]))
    }
    if (length(sel)) cols[[paths[i]]] <- sel else skipped <- c(skipped, paths[i])
  }
  if (!length(cols)) stop("No columns selected, nothing to mask.")
  paths <- names(cols)

  # 3. How IPs and locations are masked. With several files the rules can
  # differ by file (studies can have different IRB rules on what is shared).
  has_pair <- ip_cols <- list()
  for (p in paths) {
    sel <- cols[[p]]
    has_pair[[p]] <- sum(is_latitude(sel)) == 1 && sum(is_longitude(sel)) == 1
    ip_cols[[p]] <- sel[vapply(sel, is_ip_column, logical(1), data = files[[p]]$data)]
  }
  latlon_of <- modes_by_file(latlon, paths[unlist(has_pair)], paths, "latlon")
  ip_of <- modes_by_file(ip, paths[lengths(ip_cols) > 0], paths, "ip")
  undecided <- unique(c(names(latlon_of)[is.na(latlon_of)], names(ip_of)[is.na(ip_of)]))
  # The guided mode asks once for all files; different rules per file are
  # set with a list in `ip`/`latlon`.
  if (length(undecided)) {
    if (ask && batch) {
      title <- if (length(undecided) == length(paths)) sprintf("For all %d files", length(paths)) else
        paste("IP addresses and locations:", paste(basename(undecided), collapse = ", "))
      say("\n", rule(title), "\n", sep = "")
    }
    if (anyNA(latlon_of)) latlon_of[is.na(latlon_of)] <- choose_mode(NULL, "latitude/longitude", ask)
    if (anyNA(ip_of)) ip_of[is.na(ip_of)] <- choose_mode(NULL, "IP addresses", ask)
  }
  if (any(ip_of == "country_state") && is.null(ip_db)) {
    ip_db <- choose_ip_db(ask)
    if (is.null(ip_db)) ip_of[] <- "full"
  }
  if (!is.null(ip_db) && !file.exists(ip_db)) stop("IP database not found: ", ip_db)

  # 4. Where everything goes. All checks happen before anything is written.
  stamp <- time_stamp()
  out_paths <- vapply(paths, function(p) {
    output %||% sub("(\\.[^.]+)$", paste0("_masked_", stamp, "\\1"), p)
  }, character(1))
  first <- paths[1]
  report_path <- if (batch) {
    file.path(dirname(first), paste0("batch_masked_", stamp, "_report.txt"))
  } else {
    sub("\\.[^.]+$", "_report.txt", out_paths[1])
  }
  existing <- c(out_paths, report_path)[file.exists(c(out_paths, report_path))]
  if (!overwrite && length(existing)) {
    stop("Output already exists: ", paste(existing, collapse = ", "),
         "\nUse overwrite = TRUE to replace it.")
  }
  same <- normalizePath(out_paths, mustWork = FALSE) == normalizePath(paths)
  if (any(same)) stop("The output must be a different file from the original.")
  # A key is never overwritten: that would make earlier masked files
  # impossible to re-identify.
  new_key <- is.null(key_file)
  if (new_key) {
    key_file <- if (batch) {
      file.path(dirname(first), paste0("batch_KEY_", stamp, ".txt"))
    } else {
      sub("\\.[^.]+$", paste0("_KEY_", stamp, ".txt"), first)
    }
    if (file.exists(key_file)) {
      stop("A key file already exists: ", key_file,
           "\nReuse it with key_file = \"", key_file, "\", or move it elsewhere first.", call. = FALSE)
    }
    key <- derive_key(openssl::rand_bytes(32))
    key$created <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  } else {
    key <- read_key_file(key_file)
  }

  # 5. Masking, in memory
  cb <- new_codebook(key)
  db <- NULL
  if (any(ip_of == "country_state")) {
    say("Reading the IP database (this can take a minute)...\n")
    db <- read_ip_db(ip_db)
  }
  # IDs from all files are looked for in the free text of every file.
  originals <- trimws(unlist(lapply(paths, function(p) {
    lapply(cols[[p]], function(col) id_text(files[[p]]$data[[col]]))
  })))
  results <- list()
  for (i in seq_along(paths)) {
    p <- paths[i]
    if (batch) say(sprintf("Masking %s (%d of %d)...", basename(p), i, length(paths)))
    results[[p]] <- mask_one(files[[p]], cols[[p]], cb,
                             if (p %in% names(latlon_of)) latlon_of[[p]],
                             if (p %in% names(ip_of)) ip_of[[p]],
                             ip_cols[[p]], db, scan_text, mask_text, originals)
    if (batch) say(" done,", length(cols[[p]]), "column(s)\n")
  }

  # 6. Writing
  for (p in paths) {
    x <- files[[p]]
    x$data <- results[[p]]$data
    write_survey(x, out_paths[[p]])
  }
  if (new_key) {
    write_key_file(key$secret, key_file, created = key$created)
  }
  add_key_use(key_file, basename(paths))
  write_report(report_path, paths, out_paths, results, cols, key, new_key,
               ip_of, latlon_of, mask_text, skipped)

  # 7. Summary
  if (batch) {
    say("\n")
    for (p in names(results)) {
      for (note in results[[p]]$notes) tell("Note: ", basename(p), ": ", note)
    }
    if (length(skipped)) tell("Not masked (no identifier columns): ", paste(basename(skipped), collapse = ", "))
    say("\nMasked files:\n", paste0("  ", basename(out_paths), "\n"), sep = "")
    say("Report: ", basename(report_path), "\n", sep = "")
    say("Key:    ", basename(key_file), if (new_key) " (new" else " (reused", ", created ", key$created, ")\n", sep = "")
    say("Folder: ", dirname(report_path), "\n", sep = "")
  } else {
    r <- results[[1]]
    say("\nMasked", length(cols[[1]]), "column(s):", paste(cols[[1]], collapse = ", "), "\n")
    for (note in r$notes) tell("Note: ", note)
    if (!quiet && !is.null(r$text_found) && nrow(r$text_found)) print(r$text_found, row.names = FALSE)
    say("Masked file: ", out_paths[[1]], "\nReport:      ", report_path, "\n", sep = "")
    say("Key:         ", key_file, if (new_key) " (new)" else " (reused)", "\n", sep = "")
  }
  if (new_key) {
    tell("\nKEEP THE KEY FILE PRIVATE. Store it with the original data and never share it.\n",
            "It is the only way to turn the codes back into the original identifiers.")
  }
  suggested <- vapply(paths, function(p) {
    identical(cols[[p]], suggest_id_columns(files[[p]]$data)$column)
  }, logical(1))
  say("\nTo repeat this masking in one line:\n",
      repeat_command(paths, cols, suggested, ip_of, latlon_of,
                     ip_db, key_file, mask_text), "\n", sep = "")

  data_out <- lapply(results, `[[`, "data")
  if (batch) {
    invisible(list(data = data_out, output = out_paths, report = report_path, key_file = key_file))
  } else {
    invisible(list(data = data_out[[1]], output = unname(out_paths), report = report_path,
                   key_file = key_file))
  }
}

# "MSK" + region + "_" (e.g. "MSKIT_", "MSKUS-CT_"), or just "MSK" when the
# region is unknown. The code itself never contains "_" or "-", so the region
# is always the part between "MSK" and "_".
region_prefix <- function(region) ifelse(is.na(region), "MSK", paste0("MSK", region, "_"))

# Masks the chosen columns of one file. Returns the masked data, notes for the
# report, and identifiers found in the other columns.
mask_one <- function(x, columns, cb, latlon, ip, ip_cols, db, scan_text,
                     mask_text, originals) {
  data <- x$data
  notes <- character()
  done <- character()

  # Latitude and longitude are coded from the (lat, lon) pair, so the same
  # place gives the same pair of codes.
  if (!is.null(latlon)) {
    lat_col <- columns[is_latitude(columns)]
    lon_col <- columns[is_longitude(columns)]
    lat <- id_text(data[[lat_col]])
    lon <- id_text(data[[lon_col]])
    pair <- paste(trimws(lat), trimws(lon), sep = ",")
    prefix <- "MSK"
    if (latlon == "country_state") {
      region <- latlon_region(lat, lon)
      prefix <- region_prefix(region)
      notes <- c(notes, unknown_note(region, lat, "latitude/longitude"))
    }
    data[[lat_col]] <- replace_column(data[[lat_col]],
      mask_with(cb, lat, keys = paste0(lat_tag, pair), prefix = prefix))
    data[[lon_col]] <- replace_column(data[[lon_col]],
      mask_with(cb, lon, keys = paste0(lon_tag, pair), prefix = prefix))
    done <- c(lat_col, lon_col)
  }

  if (identical(ip, "country_state")) {
    for (col in ip_cols) {
      addr <- id_text(data[[col]])
      reg <- ip_region(addr, db)
      notes <- c(notes, unknown_note(reg, addr, paste0("IP addresses (", col, ")")))
      data[[col]] <- replace_column(data[[col]],
        mask_with(cb, addr, prefix = region_prefix(reg)))
      done <- c(done, col)
    }
  }

  for (col in setdiff(columns, done)) {
    data[[col]] <- replace_column(data[[col]], mask_with(cb, data[[col]]))
  }

  text_found <- NULL
  if (scan_text) {
    others <- setdiff(names(data), columns)
    text_found <- scan_text(data, others, known_ids = originals)
    if (nrow(text_found)) {
      if (mask_text) {
        for (col in unique(text_found$column)) {
          data[[col]] <- replace_column(data[[col]], mask_in_text(data[[col]], cb, originals))
        }
        notes <- c(notes, "Identifiers found inside other columns were replaced with their codes (see the report).")
      } else {
        notes <- c(notes, "Possible identifiers were found inside other columns (see the report). Check them, or rerun with mask_text = TRUE.")
      }
    }
  }

  if (x$format == "sav") {
    long <- vapply(data, function(col) nchar(attr(col, "label", exact = TRUE) %||% "", "bytes") > 256, logical(1))
    if (any(long)) notes <- c(notes, sprintf(
      "%d SPSS variable label(s) longer than 256 characters were shortened (an SPSS file limit): %s.",
      sum(long), paste(names(data)[long], collapse = ", ")))
  }
  list(data = data, notes = notes, text_found = text_found, rows = nrow(data),
       original = x$data[columns])
}

# How IPs or locations are masked in each file they apply to: a named
# character vector, NA where it is still to be asked. `value` is one mode for
# all files, or a list/vector named by file.
modes_by_file <- function(value, applies, paths, name) {
  out <- stats::setNames(rep(NA_character_, length(applies)), applies)
  if (is.null(value) || !length(applies)) return(out)
  check <- function(v) {
    if (!is.character(v) || length(v) != 1 || !v %in% c("full", "country_state")) {
      stop(name, ' must be "full" or "country_state" (or a list of these named by file).', call. = FALSE)
    }
    v
  }
  if (is.null(names(value))) {
    if (length(value) != 1) check(NA)
    out[] <- check(value[[1]])
    return(out)
  }
  bad <- setdiff(names(value), c(paths, basename(paths)))
  if (length(bad)) stop("`", name, "` names a file that is not being masked: ", paste(bad, collapse = ", "), call. = FALSE)
  for (p in applies) {
    hit <- intersect(c(p, basename(p)), names(value))
    if (length(hit)) out[[p]] <- check(value[[hit[1]]])
  }
  out
}

# Guided choice of the files: one or several, selected in one file window.
choose_files <- function(header_rows) {
  files <- list()
  for (f in unique(normalizePath(pick_files(), mustWork = FALSE))) {
    x <- tryCatch(read_survey(f, header_rows), error = function(e) {
      cat("Could not read ", basename(f), ": ", conditionMessage(e), "\n", sep = "")
      NULL
    })
    if (is.null(x)) next
    cat(sprintf("Read %s: %d rows, %d columns.\n", basename(f), nrow(x$data), ncol(x$data)))
    files[[f]] <- x
  }
  if (!length(files)) stop("None of the chosen files could be read.", call. = FALSE)
  files
}

# Opens a file dialog. Where the system offers one, several files can be
# selected at once (macOS Finder dialog, Windows dialog); otherwise R's own
# one-file dialog is used. options(surveyIDmasker.multi_select = FALSE) always
# uses R's dialog.
pick_files <- function() {
  multi <- isTRUE(getOption("surveyIDmasker.multi_select", TRUE))
  sys <- Sys.info()[["sysname"]]
  if (multi && sys == "Darwin" && nzchar(Sys.which("osascript"))) {
    cat("Choose the survey file(s) to mask (.csv, .sav, .dta or .xlsx).\n",
        "Select several with Cmd-click to mask them using the same key.\n",
        "(The window may open behind this one.)\n", sep = "")
    f <- mac_choose_files()
    if (!is.null(f)) return(f)
  } else if (multi && sys == "Windows") {
    cat("Choose the survey file(s) to mask (.csv, .sav, .dta or .xlsx).\n",
        "Select several with Ctrl-click to mask them using the same key.\n", sep = "")
    filters <- rbind(c("Survey files (*.csv, *.sav, *.dta, *.xlsx)", "*.csv;*.sav;*.dta;*.xlsx"),
                     c("All files (*.*)", "*.*"))
    f <- utils::choose.files(caption = "Choose the survey file(s) to mask",
                             multi = TRUE, filters = filters)
    if (!length(f)) quit_masking()
    return(f)
  }
  cat("Choose the survey file to mask (.csv, .sav, .dta or .xlsx)...\n")
  f <- file.choose()
  if (tolower(basename(f)) %in% quit_words && !file.exists(f)) quit_masking()
  f
}

# The macOS Finder dialog with multiple selection, through AppleScript.
# Returns NULL when the dialog cannot be shown (R's own one is used instead).
mac_choose_files <- function() {
  script <- c(
    "activate",
    "set fs to choose file with prompt \"Choose the survey file(s) to mask (Cmd-click to select several)\" with multiple selections allowed",
    "set out to \"\"",
    "repeat with f in fs",
    "set out to out & POSIX path of f & linefeed",
    "end repeat",
    "return out"
  )
  res <- suppressWarnings(system2("osascript", as.vector(rbind("-e", shQuote(script))),
                                  stdout = TRUE, stderr = TRUE))
  parse_mac_choice(res, attr(res, "status"))
}

# osascript prints one path per line; error -128 means Cancel was pressed.
parse_mac_choice <- function(out, status) {
  if (is.null(status) || status == 0) {
    f <- out[nzchar(trimws(out))]
    if (length(f)) return(f)
  }
  if (any(grepl("-128", out, fixed = TRUE))) quit_masking()
  NULL
}

# Files given as paths and/or folders; a folder stands for its survey files.
expand_files <- function(file) {
  out <- unlist(lapply(file, function(f) {
    if (dir.exists(f)) {
      found <- survey_files(f)
      if (!length(found)) stop("No survey files (.csv, .sav, .dta, .xlsx) in the folder: ", f, call. = FALSE)
      found
    } else {
      normalizePath(f, mustWork = FALSE)
    }
  }))
  unique(out)
}

# Survey files in a folder, leaving out the files this package writes.
survey_files <- function(dir) {
  f <- list.files(dir, pattern = "\\.(csv|sav|dta|xlsx)$", ignore.case = TRUE, full.names = TRUE)
  f <- f[!grepl("_(masked|reidentified)_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}|^~\\$", basename(f))]
  normalizePath(f)
}

# The `columns` argument for one file: a list named by file gives each file
# its own columns.
columns_for <- function(columns, path, paths) {
  if (!is.list(columns)) return(columns)
  hit <- intersect(c(path, basename(path)), names(columns))
  if (!length(hit)) stop("`columns` has no entry for ", basename(path), call. = FALSE)
  if (sum(basename(paths) == basename(path)) > 1 && !path %in% names(columns)) {
    stop("Several files are called ", basename(path), ": name them by full path in `columns`.", call. = FALSE)
  }
  columns[[hit[1]]]
}

# With several files a file without the given columns, or without suggested
# ones, is left out (and listed in the report) instead of stopping everything.
batch_columns <- function(data, columns, batch, name) {
  if (!batch) return(resolve_columns(data, columns))
  if (identical(columns, "suggested")) {
    found <- suggest_id_columns(data)$column
    if (!length(found)) message("No identifier columns found in ", name, ": it will not be masked.")
    return(found)
  }
  if (is.numeric(columns)) return(resolve_columns(data, columns))
  missing <- setdiff(columns, names(data))
  if (length(missing)) message("Not in ", name, ": ", paste(missing, collapse = ", "))
  if (length(missing) == length(unique(columns))) message(name, " will not be masked.")
  intersect(unique(columns), names(data))
}

rule <- function(title) {
  paste0("\u2500\u2500 ", title, " ", strrep("\u2500", max(3, 60 - nchar(title))))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Date and time added to the names of the files a run writes.
time_stamp <- function() format(Sys.time(), "%Y-%m-%d_%H-%M")

# Latitude and longitude codes encrypt the whole (lat, lon) pair, tagged so
# the two columns get different codes and reidentify() knows which half to
# return.
lat_tag <- "\037a"
lon_tag <- "\037o"

# In a batch (`batch = TRUE`) a file can be skipped: it is then left out.
choose_columns <- function(data, ask, batch = FALSE) {
  found <- suggest_id_columns(data, include_empty = TRUE)
  sugg <- found[!found$empty, ]
  empty <- found$column[found$empty]
  if (!ask) {
    if (!nrow(sugg) && batch) {
      message("No identifier columns found: this file will not be masked.")
      return(character())
    }
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
    ans <- ask_line(if (nrow(sugg)) {
      "\nNumbers of the columns to mask (e.g. 4 14 15 120-123), Enter for the suggested ones, or q to exit: "
    } else if (batch) {
      "\nNumbers of the columns to mask (e.g. 4 14 15 120-123), Enter to skip this file, or q to exit: "
    } else {
      "\nNumbers of the columns to mask (e.g. 4 14 15 120-123), or q to exit: "
    })
    if (trimws(ans) == "") {
      if (nrow(sugg)) return(sugg$column)
      if (batch) {
        cat("This file will not be masked.\n")
        return(character())
      }
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

write_report <- function(path, inputs, outputs, results, cols, key, new_key,
                         ip_of, latlon_of, mask_text, skipped) {
  mode_label <- function(m) if (identical(m, "country_state")) "country/US state kept" else "full"
  # One line in the header when every file uses the same rule; otherwise the
  # rule is given for each file.
  same_ip <- length(unique(ip_of)) <= 1
  same_loc <- length(unique(latlon_of)) <= 1
  ip <- if (length(ip_of) && same_ip) ip_of[[1]]
  latlon <- if (length(latlon_of) && same_loc) latlon_of[[1]]
  batch <- length(inputs) > 1 || length(skipped)
  key_line <- if (length(inputs) > 1) {
    sprintf("Key:             one key for all %d files, created %s (%s)",
            length(inputs), key$created, if (new_key) "new" else "existing key")
  } else {
    sprintf("Key:             created %s (%s)", key$created, if (new_key) "new" else "existing key")
  }
  lines <- c(
    "surveyIDmasker report",
    "======================",
    paste("Created:        ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Package version:", as.character(utils::packageVersion("surveyIDmasker"))),
    key_line,
    if (!is.null(ip)) paste("IP addresses:   ", mode_label(ip)),
    if (!same_ip) "IP addresses:    differ by file (see each file)",
    if (!is.null(latlon)) paste("Locations:      ", mode_label(latlon), "(latitude/longitude)"),
    if (!same_loc) "Locations:       differ by file (see each file)",
    "Codes:           encrypted with a private key kept in a separate file",
    "Blank values:    MSKblank"
  )
  for (i in seq_along(inputs)) {
    p <- inputs[i]
    r <- results[[p]]
    pad <- if (batch) "   " else ""
    lines <- c(lines, "",
      if (batch) sprintf("%d. %s -> %s", i, basename(p), basename(outputs[[p]])) else
        c(paste("Original file:  ", basename(p)), paste("Masked file:    ", basename(outputs[[p]]))),
      paste0(pad, "Rows: ", r$rows),
      paste0(pad, "Masked columns: ", paste(cols[[p]], collapse = ", ")),
      if (!same_ip && p %in% names(ip_of)) paste0(pad, "IP addresses: ", mode_label(ip_of[[p]])),
      if (!same_loc && p %in% names(latlon_of)) paste0(pad, "Locations: ", mode_label(latlon_of[[p]]), " (latitude/longitude)"))
    if (length(r$notes)) lines <- c(lines, paste0(pad, "Note: ", r$notes))
    tf <- r$text_found
    if (!is.null(tf) && nrow(tf)) {
      lines <- c(lines, sprintf("%sPossible identifiers in other columns (%s):", pad,
                                if (mask_text) "masked" else "NOT masked"),
                 sprintf("%s  %-30s %-18s %d cell(s)", pad, tf$column, tf$type, tf$cells))
    }
    lines <- c(lines, paste0(pad, "SHA-256 of masked file: ",
                             digest::digest(file = outputs[[p]], algo = "sha256")))
  }
  if (length(skipped)) {
    lines <- c(lines, "", "Not masked (no identifier columns):", paste0("  ", basename(skipped)))
  }
  sentence <- methods_sentence(inputs, results, cols, ip_of, latlon_of)
  lines <- c(lines, "", "For the methods section or README (edit as needed):",
             paste0("  ", strwrap(sentence, 76)), "", "Reference:", paste0("  ", strwrap(citation_text(), 76)))
  writeLines(lines, path)
}

# A sentence describing the masking, ready to paste into a paper or a
# replication package README.
methods_sentence <- function(inputs, results, cols, ip_of, latlon_of) {
  kinds <- unique(unlist(lapply(inputs, function(p) {
    vapply(cols[[p]], function(col) column_kind(col, results[[p]]$original[[col]]), character(1))
  })))
  kinds <- c(setdiff(kinds, "other identifiers"), intersect(kinds, "other identifiers"))
  what <- if (length(kinds) > 1) {
    paste0(paste(kinds[-length(kinds)], collapse = ", "), " and ", kinds[length(kinds)])
  } else kinds
  n <- length(inputs)
  what <- if (identical(kinds, "other identifiers")) "" else paste0(" (", what, ")")
  s <- sprintf(paste0("Participant identifiers%s were replaced with codes encrypted with a private key, ",
                      "using surveyIDmasker %s (Zanibellato, 2026); the same identifier received the same code%s."),
               what, as.character(utils::packageVersion("surveyIDmasker")),
               if (n == 2) " in both data files" else if (n > 2) sprintf(" in all %d data files", n) else "")
  kept <- function(modes, what) {
    if (!length(modes) || !any(modes == "country_state")) return(NULL)
    where <- if (all(modes == "country_state")) "" else " in some files"
    sprintf("Masked %s keep the country (and the US state)%s.", what, where)
  }
  paste(c(s, kept(latlon_of, "locations"), kept(ip_of, "IP addresses")), collapse = " ")
}

# Plain-language kind of a masked column, from its name or content.
column_kind <- function(col, values) {
  reason <- id_column_names[normalize_name(col)]
  if (is.na(reason)) {
    x <- trimws(as.character(values))
    x <- x[!is_blank(x)]
    for (type in names(id_patterns)) {
      if (length(x) && mean(grepl(id_patterns[[type]], x, perl = TRUE)) >= 0.5) reason <- type
    }
  }
  if (is.na(reason)) return("other identifiers")
  reason <- unname(reason)
  plural <- c("location (latitude)" = "locations", "location (longitude)" = "locations",
              "location (zip code)" = "zip codes", "location (postal code)" = "postal codes",
              "location (city)" = "cities", "location (region)" = "regions",
              "location (province)" = "provinces", "IP address" = "IP addresses",
              "email" = "emails", "name" = "names", "phone number" = "phone numbers",
              "phone area code" = "phone area codes", "completion code" = "completion codes",
              "external reference" = "external references")
  if (reason %in% names(plural)) return(unname(plural[reason]))
  sub(" ID$", " IDs", reason)
}

#' List the columns of a survey file with their numbers
#'
#' Use the numbers in `mask_survey(columns = ...)`. Suggested identifier
#' columns are marked with `*`; identifier columns with no data also show
#' `(empty)`.
#'
#' @param file Path to a survey data file (.csv, .sav, .dta or .xlsx).
#' @return Invisibly, a data frame with `number`, `column`, `suggested` and
#'   `empty`.
#' @examples
#' example <- system.file("extdata", "example_survey.csv", package = "surveyIDmasker")
#' list_columns(example)
#' @export
list_columns <- function(file) {
  data <- read_survey(file)$data
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
                         "Keep country, and state for the US (e.g. MSKUS-CT_..., MSKIT_...)"))
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

repeat_command <- function(paths, cols, suggested, ip_of, latlon_of, ip_db, key_file, mask_text) {
  q <- function(x) paste0('"', gsub('"', '\\\\"', x), '"')
  vec <- function(x) if (length(x) == 1) q(x) else paste0("c(", paste(q(x), collapse = ", "), ")")
  one_col <- function(p) if (suggested[[p]]) q("suggested") else vec(cols[[p]])
  if (length(paths) == 1) {
    file_arg <- q(paths)
    col_arg <- paste0("columns = ", if (suggested[[1]]) q("suggested") else vec(cols[[1]]))
  } else {
    file_arg <- paste0("c(", paste(q(paths), collapse = ",\n    "), ")")
    col_arg <- if (all(suggested)) {
      'columns = "suggested"'
    } else {
      nm <- if (anyDuplicated(basename(paths))) paths else basename(paths)
      paste0("columns = list(\n      ",
             paste(paste0(q(nm), " = ", vapply(paths, one_col, character(1))), collapse = ",\n      "),
             ")")
    }
  }
  args <- c(file_arg, col_arg,
            mode_arg("ip", ip_of),
            mode_arg("latlon", latlon_of),
            if (!is.null(ip_db)) paste0("ip_db = ", q(ip_db)),
            paste0("key_file = ", q(key_file)),
            if (mask_text) "mask_text = TRUE")
  paste0("mask_survey(", paste(args, collapse = ",\n  "), ")")
}

# `ip = "full"` when every file uses the same rule, otherwise a list by file.
mode_arg <- function(name, modes) {
  if (!length(modes)) return(NULL)
  q <- function(x) paste0('"', x, '"')
  if (length(unique(modes)) == 1) return(paste0(name, " = ", q(modes[[1]])))
  nm <- if (anyDuplicated(basename(names(modes)))) names(modes) else basename(names(modes))
  paste0(name, " = list(", paste(paste0(q(nm), " = ", q(modes)), collapse = ", "), ")")
}
