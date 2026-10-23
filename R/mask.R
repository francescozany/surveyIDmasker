code_alphabet <- c(LETTERS, letters, 0:9)
tag_length <- 8
# Values are padded to at least this many bytes before encryption, so every
# code has at least 35 characters and short IDs don't stand out by length.
min_plain_bytes <- 18

# Codes are the original value encrypted with the key (AES-256 in a
# deterministic "synthetic IV" construction): the IV is an HMAC of the value,
# so the same value always gives the same code, and the IV doubles as a check
# that a code was made with this key. Only the key can decrypt a code.
encrypt_code <- function(value, key) {
  pt <- pad(charToRaw(enc2utf8(value)))
  tag <- digest::hmac(key$mac, pt, algo = "sha256", raw = TRUE)[seq_len(tag_length)]
  ct <- openssl::aes_ctr_encrypt(pt, key$enc, iv = c(tag, as.raw(rep(0, 16 - tag_length))))
  raw_to_base62(c(tag, ct))
}

# Returns the original value, or NA when the code was not made with this key.
decrypt_code <- function(code, key) {
  bytes <- base62_to_raw(code)
  if (is.null(bytes) || length(bytes) <= tag_length) return(NA_character_)
  tag <- bytes[seq_len(tag_length)]
  ct <- bytes[-seq_len(tag_length)]
  pt <- openssl::aes_ctr_decrypt(ct, key$enc, iv = c(tag, as.raw(rep(0, 16 - tag_length))))
  check <- digest::hmac(key$mac, pt, algo = "sha256", raw = TRUE)[seq_len(tag_length)]
  if (!identical(check, tag)) return(NA_character_)
  pt <- unpad(pt)
  if (is.null(pt)) return(NA_character_)
  rawToChar(pt)
}

# ISO/IEC 7816-4 padding: a 0x80 byte, then zeros up to the minimum length.
pad <- function(bytes) {
  bytes <- c(bytes, as.raw(0x80))
  c(bytes, as.raw(rep(0, max(0, min_plain_bytes - length(bytes)))))
}

unpad <- function(bytes) {
  nz <- which(bytes != as.raw(0))
  if (!length(nz) || bytes[max(nz)] != as.raw(0x80)) return(NULL)
  bytes[seq_len(max(nz) - 1)]
}

# Base-62 (A-Z, a-z, 0-9) so codes stay alphanumeric. A leading 0x01 byte
# keeps leading zero bytes from being lost.
raw_to_base62 <- function(bytes) {
  n <- openssl::bignum(c(as.raw(1), bytes))
  zero <- openssl::bignum(0)
  base <- openssl::bignum(62)
  digits <- integer()
  while (n > zero) {
    digits <- c(as.integer(as.character(n %% base)), digits)
    n <- n %/% base
  }
  paste(code_alphabet[digits + 1], collapse = "")
}

base62_to_raw <- function(code) {
  d <- match(strsplit(code, "")[[1]], code_alphabet) - 1
  if (!length(d) || anyNA(d)) return(NULL)
  n <- openssl::bignum(0)
  base <- openssl::bignum(62)
  for (x in d) n <- n * base + openssl::bignum(x)
  bytes <- as.raw(n)
  if (!length(bytes) || bytes[1] != as.raw(1)) return(NULL)
  bytes[-1]
}

# A codebook caches one code per distinct value for the whole run, so the
# same value gets the same code in every column without encrypting it twice.
new_codebook <- function(key) {
  cb <- new.env(parent = emptyenv())
  cb$key <- key
  cb$map <- character()
  cb
}

codes_for <- function(cb, keys) {
  new <- setdiff(unique(keys), names(cb$map))
  if (length(new)) {
    codes <- vapply(new, encrypt_code, character(1), key = cb$key, USE.NAMES = FALSE)
    cb$map <- c(cb$map, stats::setNames(codes, new))
  }
  unname(cb$map[keys])
}

is_blank <- function(x) is.na(x) | trimws(x) == ""

#' Mask a vector of identifiers
#'
#' Replaces every non-blank value with `"MSK"` plus a code made by encrypting
#' the value with a key. Equal values get equal codes; blank or missing values
#' become `"MSKblank"`. With the same key, [reidentify()] turns the codes back
#' into the original values.
#'
#' @param x A vector of identifiers.
#' @param key_file Path to a key file made by [mask_qualtrics()] or
#'   [new_key_file()]. If `NULL`, a temporary key is used and the codes cannot
#'   be turned back.
#' @param prefix Prefix(es) placed before the code, recycled along `x`.
#' @return A character vector of masked values.
#' @examples
#' mask_values(c("A1B2C3", "", "A1B2C3", "XYZ"))
#' @export
mask_values <- function(x, key_file = NULL, prefix = "MSK") {
  key <- if (is.null(key_file)) derive_key(openssl::rand_bytes(32)) else read_key_file(key_file)
  mask_with(new_codebook(key), x, prefix = prefix)
}

mask_with <- function(cb, x, keys = trimws(as.character(x)), prefix = "MSK") {
  x <- as.character(x)
  prefix <- rep_len(prefix, length(x))
  out <- rep("MSKblank", length(x))
  ok <- !is_blank(x)
  out[ok] <- paste0(prefix[ok], codes_for(cb, keys[ok]))
  out
}
