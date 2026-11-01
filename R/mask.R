code_alphabet <- c(LETTERS, letters, 0:9)
tag_length <- 8
# Values are padded to at least this many bytes before encryption, so every
# code has at least 35 characters and short IDs don't stand out by length.
min_plain_bytes <- 18

# Codes are the original value encrypted with the key (AES-256 in a
# deterministic "synthetic IV" construction): the IV is an HMAC of the value,
# so the same value always gives the same code, and the IV doubles as a check
# that a code was made with this key. Only the key can decrypt a code.
encrypt_code <- function(value, key) encrypt_codes(value, key)

encrypt_codes <- function(values, key) {
  bytes <- lapply(values, function(value) {
    pt <- pad(charToRaw(enc2utf8(value)))
    tag <- hmac(key$mac, pt)[seq_len(tag_length)]
    ct <- openssl::aes_ctr_encrypt(pt, key$enc, iv = c(tag, as.raw(rep(0, 16 - tag_length))))
    c(tag, ct)
  })
  raw_to_base62(bytes)
}

# Returns the original value, or NA when the code was not made with this key.
decrypt_code <- function(code, key) decrypt_codes(code, key)

decrypt_codes <- function(codes, key) {
  if (!length(codes)) return(character())
  vapply(base62_to_raw(codes), decrypt_bytes, character(1), key = key)
}

decrypt_bytes <- function(bytes, key) {
  if (is.null(bytes) || length(bytes) <= tag_length) return(NA_character_)
  tag <- bytes[seq_len(tag_length)]
  ct <- bytes[-seq_len(tag_length)]
  pt <- openssl::aes_ctr_decrypt(ct, key$enc, iv = c(tag, as.raw(rep(0, 16 - tag_length))))
  check <- hmac(key$mac, pt)[seq_len(tag_length)]
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
# keeps leading zero bytes from being lost. Both directions work on many
# values at once.
raw_to_base62 <- function(bytes) {
  if (is.raw(bytes)) bytes <- list(bytes)
  digits <- change_base(lapply(bytes, function(b) c(1L, as.integer(b))), 256L, 62L)
  vapply(digits, function(d) paste(code_alphabet[d + 1], collapse = ""), character(1))
}

# A list with the bytes of each code, NULL for text that is not a code.
base62_to_raw <- function(codes) {
  d <- lapply(strsplit(codes, "", fixed = TRUE), function(x) match(x, code_alphabet) - 1L)
  ok <- vapply(d, function(x) length(x) > 0 && !anyNA(x), logical(1))
  out <- vector("list", length(codes))
  if (!any(ok)) return(out)
  bytes <- change_base(d[ok], 62L, 256L)
  out[ok] <- lapply(bytes, function(b) if (length(b) && b[1] == 1L) as.raw(b[-1]) else NULL)
  out
}

# Converts numbers written as digit vectors (most significant first) from one
# base to another, by long division. Numbers with the same number of digits
# are divided together, one column at a time.
change_base <- function(numbers, from, to) {
  out <- vector("list", length(numbers))
  len <- lengths(numbers)
  for (n in unique(len)) {
    rows <- which(len == n)
    m <- matrix(unlist(numbers[rows]), ncol = n, byrow = TRUE)
    digits <- list()
    alive <- rowSums(m) > 0
    while (any(alive)) {
      carry <- integer(nrow(m))
      for (j in seq_len(n)) {
        cur <- carry * from + m[, j]
        m[, j] <- cur %/% to
        carry <- cur %% to
      }
      digits[[length(digits) + 1]] <- ifelse(alive, carry, NA_integer_)
      alive <- rowSums(m) > 0
    }
    d <- do.call(cbind, rev(digits))
    if (is.null(d)) d <- matrix(integer(), nrow(m), 0)
    out[rows] <- lapply(seq_len(nrow(d)), function(i) { x <- d[i, ]; x[!is.na(x)] })
  }
  out
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
    cb$map <- c(cb$map, stats::setNames(encrypt_codes(new, cb$key), new))
  }
  unname(cb$map[keys])
}

is_blank <- function(x) is.na(x) | trimws(x) == ""

# Values as text. Numbers are written in full ("100000", not "1e+05"), so an
# ID stored as a number in SPSS, Stata or Excel gets the same code as the same
# ID written in a CSV.
id_text <- function(x) {
  if (!is.numeric(x)) return(as.character(x))
  vapply(unclass(x), function(v) {
    if (is.na(v)) NA_character_ else format(v, scientific = FALSE, digits = 15, trim = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

#' Mask a vector of identifiers
#'
#' Replaces every non-blank value with `"MSK"` plus a code made by encrypting
#' the value with a key. Equal values get equal codes; blank or missing values
#' become `"MSKblank"`. With the same key, [reidentify()] turns the codes back
#' into the original values.
#'
#' @param x A vector of identifiers.
#' @param key_file Path to a key file made by [mask_survey()] or
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

mask_with <- function(cb, x, keys = NULL, prefix = "MSK") {
  x <- id_text(x)
  if (is.null(keys)) keys <- trimws(x)
  prefix <- rep_len(prefix, length(x))
  out <- rep("MSKblank", length(x))
  ok <- !is_blank(x)
  out[ok] <- paste0(prefix[ok], codes_for(cb, keys[ok]))
  out
}
