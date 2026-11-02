# HMAC-SHA256, as raw bytes.
hmac <- function(key, message) as.raw(openssl::sha256(message, key = key))

# The key file holds one random 256-bit secret. Separate keys for encryption
# and for the integrity check are derived from it.
derive_key <- function(secret) {
  list(secret = secret,
       enc = hmac(secret, charToRaw("surveyIDmasker-encrypt")),
       mac = hmac(secret, charToRaw("surveyIDmasker-check")))
}

#' Create a new key file
#'
#' Makes a new random key and saves it as a small text file. Anyone who has
#' this file can turn masked codes back into the original identifiers, so keep
#' it private and never share it with the data. [mask_survey()] creates one
#' automatically; use this only to prepare a key in advance.
#'
#' @param path Where to save the key file.
#' @param overwrite Replace an existing file.
#' @return Invisibly, the path.
#' @examples
#' key <- new_key_file(tempfile(fileext = ".txt"))
#' mask_values(c("A1B2C3D4E5F6G7", "B2C3D4E5F6G7H8"), key_file = key)
#' @export
new_key_file <- function(path, overwrite = FALSE) {
  if (file.exists(path) && !overwrite) stop("Key file already exists: ", path)
  write_key_file(openssl::rand_bytes(32), path)
}

write_key_file <- function(secret, path, created = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")) {
  lines <- c(
    "# surveyIDmasker KEY - KEEP THIS FILE PRIVATE.",
    "# Anyone with this file can turn the masked codes back into the original",
    "# identifiers. Never share it with the masked data; store it with the",
    "# original data. Without it, the masked codes cannot be turned back.",
    paste("# Created:", created),
    paste0("key: ", paste(as.character(secret), collapse = "")),
    "# Files masked with this key:"
  )
  writeLines(lines, path)
  invisible(path)
}

# Adds the files just masked to the history kept in the key file.
add_key_use <- function(path, files) {
  cat(sprintf("#   %s  %s\n", format(Sys.time(), "%Y-%m-%d %H:%M"), files),
      file = path, append = TRUE, sep = "")
}

read_key_file <- function(path) {
  if (!file.exists(path)) stop("Key file not found: ", path)
  line <- grep("^key: *[0-9a-f]{64} *$", readLines(path, warn = FALSE), value = TRUE)
  if (length(line) != 1) stop("This does not look like a surveyIDmasker key file: ", path)
  hex <- sub("^key: *([0-9a-f]{64}) *$", "\\1", line)
  secret <- as.raw(strtoi(substring(hex, seq(1, 63, 2), seq(2, 64, 2)), 16L))
  key <- derive_key(secret)
  created <- grep("^# Created:", readLines(path, warn = FALSE), value = TRUE)
  key$created <- if (length(created)) trimws(sub("^# Created:", "", created[1])) else "(unknown)"
  key
}
