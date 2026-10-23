# The key file holds one random 256-bit secret. Separate keys for encryption
# and for the integrity check are derived from it.
derive_key <- function(secret) {
  list(secret = secret,
       enc = digest::hmac(secret, "surveyIDmasker-encrypt", algo = "sha256", raw = TRUE),
       mac = digest::hmac(secret, "surveyIDmasker-check", algo = "sha256", raw = TRUE))
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
#' @export
new_key_file <- function(path, overwrite = FALSE) {
  if (file.exists(path) && !overwrite) stop("Key file already exists: ", path)
  write_key_file(openssl::rand_bytes(32), path)
}

write_key_file <- function(secret, path, used_for = NULL) {
  lines <- c(
    "# surveyIDmasker KEY - KEEP THIS FILE PRIVATE.",
    "# Anyone with this file can turn the masked codes back into the original",
    "# identifiers. Never share it with the masked data; store it with the",
    "# original data. Without it, the masked codes cannot be turned back.",
    paste("# Created:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    if (!is.null(used_for)) paste("# First used for:", used_for),
    paste0("key: ", paste(as.character(secret), collapse = ""))
  )
  writeLines(lines, path)
  invisible(path)
}

read_key_file <- function(path) {
  if (!file.exists(path)) stop("Key file not found: ", path)
  line <- grep("^key: *[0-9a-f]{64} *$", readLines(path, warn = FALSE), value = TRUE)
  if (length(line) != 1) stop("This does not look like a surveyIDmasker key file: ", path)
  hex <- sub("^key: *([0-9a-f]{64}) *$", "\\1", line)
  secret <- as.raw(strtoi(substring(hex, seq(1, 63, 2), seq(2, 64, 2)), 16L))
  derive_key(secret)
}
