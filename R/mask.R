code_alphabet <- c(LETTERS, letters, 0:9)
code_length <- 10

# Turns raw bytes into a base-62 code. Bytes >= 248 are dropped so every
# character of the alphabet is equally likely (248 = 4 * 62).
bytes_to_code <- function(bytes, n = code_length) {
  b <- as.integer(bytes)
  b <- b[b < 248]
  if (length(b) < n) return(NA_character_)
  paste(code_alphabet[b[seq_len(n)] %% 62 + 1], collapse = "")
}

random_code <- function() {
  repeat {
    code <- bytes_to_code(openssl::rand_bytes(24))
    if (!is.na(code)) return(code)
  }
}

seeded_code <- function(value, seed) {
  i <- 0
  repeat {
    msg <- if (i == 0) value else paste0(value, "#", i)
    code <- bytes_to_code(digest::hmac(seed, msg, algo = "sha256", raw = TRUE))
    if (!is.na(code)) return(code)
    i <- i + 1
  }
}

# A codebook keeps one code per distinct value for the whole run, so the same
# value gets the same code in every column. It lives only in memory.
new_codebook <- function(seed = NULL) {
  cb <- new.env(parent = emptyenv())
  cb$seed <- if (is.null(seed)) NULL else as.character(seed)
  cb$map <- character()
  cb
}

codes_for <- function(cb, keys) {
  new <- setdiff(unique(keys), names(cb$map))
  if (length(new)) {
    codes <- if (is.null(cb$seed)) {
      vapply(new, function(k) random_code(), character(1), USE.NAMES = FALSE)
    } else {
      vapply(new, seeded_code, character(1), seed = cb$seed, USE.NAMES = FALSE)
    }
    # With 62^10 possible codes a clash is practically impossible, but never
    # let two different values share a code.
    clash <- duplicated(c(cb$map, codes))[length(cb$map) + seq_along(codes)]
    while (any(clash)) {
      if (!is.null(cb$seed)) stop("Two values produced the same code; try a different seed.")
      codes[clash] <- vapply(which(clash), function(i) random_code(), character(1))
      clash <- duplicated(c(cb$map, codes))[length(cb$map) + seq_along(codes)]
    }
    cb$map <- c(cb$map, stats::setNames(codes, new))
  }
  unname(cb$map[keys])
}

is_blank <- function(x) is.na(x) | trimws(x) == ""

#' Mask a vector of identifiers
#'
#' Replaces every non-blank value with `"MSK"` plus a 10-character
#' alphanumeric code. Equal values get equal codes; blank or missing values
#' become `"MSKblank"`.
#'
#' @param x A vector of identifiers.
#' @param seed Optional seed (a long phrase is best). With the same seed, the
#'   same value always gets the same code, across runs and files. Without it,
#'   codes are random and cannot be reproduced.
#' @param prefix Prefix(es) placed before the code, recycled along `x`.
#' @return A character vector of masked values.
#' @examples
#' mask_values(c("A1B2C3", "", "A1B2C3", "XYZ"))
#' @export
mask_values <- function(x, seed = NULL, prefix = "MSK") {
  mask_with(new_codebook(seed), x, prefix = prefix)
}

mask_with <- function(cb, x, keys = trimws(as.character(x)), prefix = "MSK") {
  x <- as.character(x)
  prefix <- rep_len(prefix, length(x))
  out <- rep("MSKblank", length(x))
  ok <- !is_blank(x)
  out[ok] <- paste0(prefix[ok], codes_for(cb, keys[ok]))
  out
}
