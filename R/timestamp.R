# Independent timestamps for reports (RFC 3161). Only the SHA-256 fingerprint
# of the report is sent to a public timestamping service, which returns a
# signed token saying when it saw that fingerprint. The report holds the
# fingerprint of every masked file, so the token covers them too.

# Services tried in order; both sign with certificates that lead to
# authorities in the standard list of trusted roots (openssl::ca_bundle()).
# DigiCert answers only over http, which is safe here: the reply is signed and
# checked before it is kept.
tsa_services <- c(
  Sectigo = "https://timestamp.sectigo.com",
  DigiCert = "http://timestamp.digicert.com"
)

# The timestamp is added at the end of the report, between these two lines.
# It covers everything above it.
ts_begin <- "----- Timestamp (do not edit) -----"
ts_end <- "----- End of timestamp -----"

# Asks a service for a timestamp of the file at `path` and adds it at the end
# of the file. Returns the name of the service, or NULL when none answered.
stamp_file <- function(path, services = tsa_services) {
  hash <- file_hash(path)
  nonce <- openssl::rand_bytes(8)
  nonce[1] <- (nonce[1] & as.raw(0x7f)) | as.raw(0x01)
  for (name in names(services)) {
    resp <- tryCatch(request_timestamp(services[[name]], ts_request(hash, nonce)),
                     error = function(e) NULL)
    if (is.null(resp)) next
    # Kept only if it is a valid timestamp of this very file.
    check <- tryCatch(read_timestamp(resp), error = function(e) NULL)
    if (is.null(check) || !identical(check$imprint, hash) || !identical(check$nonce, nonce)) next
    if (!isTRUE(verify_token(check)$valid)) next
    append_timestamp(path, resp)
    return(name)
  }
  NULL
}

request_timestamp <- function(url, request) {
  h <- curl::new_handle(post = TRUE, postfields = request, postfieldsize = length(request),
                        timeout = 20, connecttimeout = 10)
  curl::handle_setheaders(h, "Content-Type" = "application/timestamp-query")
  r <- curl::curl_fetch_memory(url, h)
  if (r$status_code != 200) return(NULL)
  r$content
}

file_hash <- function(path) as.raw(openssl::sha256(readBin(path, "raw", file.size(path))))

# Adds the timestamp (base64 text, 64 characters per line) at the end of the
# file, with the same line endings as the file.
append_timestamp <- function(path, token) {
  raw <- readBin(path, "raw", file.size(path))
  nl <- if (length(grepRaw("\r\n", raw, fixed = TRUE))) "\r\n" else "\n"
  b64 <- openssl::base64_encode(token)
  starts <- seq(1, nchar(b64), by = 64)
  block <- paste0(paste(c(ts_begin, substring(b64, starts, starts + 63), ts_end), collapse = nl), nl)
  con <- file(path, "ab")
  on.exit(close(con))
  writeBin(charToRaw(block), con)
}

# Splits a report into the part that was timestamped and the timestamp at its
# end. `token` is NULL when there is no timestamp, `damaged` is TRUE when the
# timestamp lines cannot be read, and `added` is TRUE when there is text after
# the timestamp (which it does not cover).
split_timestamp <- function(path) {
  raw <- readBin(path, "raw", file.size(path))
  at <- grepRaw(ts_begin, raw, fixed = TRUE, all = TRUE)
  # The block starts a line.
  at <- at[at == 1 | raw[pmax(at - 1, 1)] == as.raw(0x0a)]
  if (!length(at)) return(list(content = raw, token = NULL, damaged = FALSE, added = FALSE))
  at <- at[length(at)]
  lines <- strsplit(rawToChar(raw[at:length(raw)]), "\r?\n")[[1]]
  end <- match(ts_end, lines)
  token <- if (!is.na(end) && end > 2) {
    tryCatch(openssl::base64_decode(paste(lines[2:(end - 1)], collapse = "")),
             error = function(e) NULL)
  }
  added <- !is.na(end) && any(nzchar(trimws(lines[-seq_len(end)])))
  list(content = raw[seq_len(at - 1)], token = token, damaged = is.null(token), added = added)
}

# --- The request: a small DER structure (RFC 3161, TimeStampReq) -------------

ts_request <- function(hash, nonce) {
  der(0x30,
      der(0x02, as.raw(1)),
      der(0x30, der(0x30, der(0x06, oid_bytes$sha256), as.raw(c(0x05, 0x00))),
          der(0x04, hash)),
      der(0x02, nonce),
      der(0x01, as.raw(0xff)))   # ask for the service's certificates
}

der <- function(tag, ...) {
  value <- c(...)
  n <- length(value)
  len <- if (n < 128) as.raw(n) else {
    b <- as.raw(n %/% 256^(3:0) %% 256)
    b <- b[which(b != as.raw(0))[1]:4]
    c(as.raw(0x80 + length(b)), b)
  }
  c(as.raw(tag), len, value)
}

# --- Reading DER --------------------------------------------------------------

# The element starting at position i of x: its tag, the whole element (tlv),
# its content (value) and where the next element starts.
der_read <- function(x, i = 1L) {
  if (i + 1L > length(x)) stop("damaged timestamp file")
  tag <- as.integer(x[i])
  n <- as.integer(x[i + 1L])
  h <- 2L
  if (n >= 128L) {
    k <- n - 128L
    n <- sum(as.integer(x[i + 1L + seq_len(k)]) * 256^((k - 1L):0))
    h <- 2L + k
  }
  end <- i + h + n - 1
  if (end > length(x)) stop("damaged timestamp file")
  list(tag = tag, tlv = x[i:end], value = x[seq.int(i + h, length.out = n)], next_at = end + 1)
}

der_items <- function(x) {
  out <- list()
  i <- 1L
  while (i <= length(x)) {
    e <- der_read(x, i)
    out[[length(out) + 1]] <- e
    i <- e$next_at
  }
  out
}

oid_bytes <- list(
  sha1 = as.raw(c(0x2b, 0x0e, 0x03, 0x02, 0x1a)),
  sha256 = as.raw(c(0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x01)),
  sha384 = as.raw(c(0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x02)),
  sha512 = as.raw(c(0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x03)),
  message_digest = as.raw(c(0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x09, 0x04)),
  time_stamping = as.raw(c(0x06, 0x08, 0x2b, 0x06, 0x01, 0x05, 0x05, 0x07, 0x03, 0x08))
)

# The hash function named by an algorithm identifier (digest or signature
# algorithm). NULL when the identifier names no hash (e.g. rsaEncryption).
hash_of <- function(oid) {
  hex <- paste(as.character(oid), collapse = "")
  switch(hex,
    "2b0e03021a" = , "2a864886f70d010105" = openssl::sha1,
    "608648016503040201" = , "2a864886f70d01010b" = , "2a8648ce3d040302" = openssl::sha256,
    "608648016503040202" = , "2a864886f70d01010c" = , "2a8648ce3d040303" = openssl::sha384,
    "608648016503040203" = , "2a864886f70d01010d" = , "2a8648ce3d040304" = openssl::sha512,
    NULL)
}

# The parts of a timestamp file (TimeStampResp) needed to check it.
read_timestamp <- function(resp) {
  top <- der_items(der_read(resp)$value)
  status <- as.integer(der_items(top[[1]]$value)[[1]]$value)
  if (!status %in% 0:1 || length(top) < 2) stop("the service refused the timestamp")
  content_info <- der_items(top[[2]]$value)
  signed <- der_items(der_read(content_info[[2]]$value)$value)
  encap <- der_items(signed[[3]]$value)
  tst_der <- der_read(encap[[2]]$value)$value   # the TSTInfo, as signed
  tst <- der_items(der_read(tst_der)$value)
  imprint <- der_items(tst[[3]]$value)
  gen_at <- which(vapply(tst, `[[`, integer(1), "tag") == 0x18)[1]
  after <- tst[-seq_len(gen_at)]
  nonce <- Filter(function(e) e$tag == 0x02, after)
  certs <- Filter(function(e) e$tag == 0xa0, signed)
  certs <- if (length(certs)) lapply(der_items(certs[[1]]$value), `[[`, "tlv") else list()
  signer <- der_items(der_items(signed[[length(signed)]]$value)[[1]]$value)
  tags <- vapply(signer, `[[`, integer(1), "tag")
  attrs <- signer[[which(tags == 0xa0)[1]]]
  list(
    imprint = imprint[[2]]$value,
    time = der_time(tst[[gen_at]]$value),
    nonce = if (length(nonce)) nonce[[1]]$value,
    tst = tst_der,
    certs = certs,
    digest_alg = der_items(signer[[3]]$value)[[1]]$value,
    sig_alg = der_items(signer[[which(tags == 0xa0)[1] + 1]]$value)[[1]]$value,
    signature = signer[[which(tags == 0x04)[1]]]$value,
    attrs = c(as.raw(0x31), attrs$tlv[-1]),   # signed as a SET
    message_digest = attr_value(attrs$value, oid_bytes$message_digest)
  )
}

attr_value <- function(attrs, oid) {
  for (a in der_items(attrs)) {
    parts <- der_items(a$value)
    if (identical(parts[[1]]$value, oid)) return(der_read(parts[[2]]$value)$value)
  }
  NULL
}

# UTCTime or GeneralizedTime as POSIXct (UTC).
der_time <- function(x) {
  s <- rawToChar(x)
  if (nchar(sub("[.Z].*$", "", s)) == 12) {
    s <- paste0(if (as.integer(substr(s, 1, 2)) < 50) "20" else "19", s)
  }
  as.POSIXct(substr(s, 1, 14), format = "%Y%m%d%H%M%S", tz = "UTC")
}

# --- Checking a timestamp -----------------------------------------------------

# Whether the token is signed by a timestamping service whose certificate leads
# to a trusted authority, with every certificate valid at the time of the stamp.
verify_token <- function(ts) {
  fail <- function(why) list(valid = FALSE, why = why, service = NA_character_)
  hash <- hash_of(ts$digest_alg)
  if (is.null(hash)) return(fail("unknown hash algorithm"))
  if (!identical(ts$message_digest, as.raw(hash(ts$tst)))) return(fail("the signed content does not match"))
  sig_hash <- hash_of(ts$sig_alg) %||% hash
  signer <- NULL
  for (cert in ts$certs) {
    ok <- tryCatch(openssl::signature_verify(ts$attrs, ts$signature, hash = sig_hash,
                                             pubkey = as.list(read_cert_der(cert))$pubkey),
                   error = function(e) FALSE)
    if (isTRUE(ok)) { signer <- cert; break }
  }
  if (is.null(signer)) return(fail("the signature is not valid"))
  if (is.null(grepRaw(oid_bytes$time_stamping, signer, fixed = TRUE))) {
    return(fail("the certificate is not one for timestamps"))
  }
  chain <- trust_chain(signer, ts$certs, ts$time)
  if (!isTRUE(chain)) return(fail(chain))
  list(valid = TRUE, why = NA_character_, service = cert_name(signer))
}

read_cert_der <- function(der) openssl::read_cert(der, der = TRUE)

# The parts of a certificate needed to follow the chain.
cert_parts <- function(der) {
  top <- der_items(der_read(der)$value)
  tbs <- der_items(top[[1]]$value)
  if (tbs[[1]]$tag == 0xa0) tbs <- tbs[-1]
  validity <- der_items(tbs[[4]]$value)
  list(tbs = top[[1]]$tlv,
       sig_alg = der_items(top[[2]]$value)[[1]]$value,
       signature = top[[3]]$value[-1],
       issuer = tbs[[3]]$tlv,
       subject = tbs[[5]]$tlv,
       from = der_time(validity[[1]]$value),
       to = der_time(validity[[2]]$value))
}

# TRUE, or why the chain from the signing certificate to a trusted authority
# fails. Certificates are checked at the time of the stamp, so a timestamp
# stays valid after the service's certificate expires.
trust_chain <- function(signer, certs, at) {
  roots <- openssl::ca_bundle()
  pool <- c(certs, lapply(roots, unclass))
  root_keys <- vapply(roots, function(r) key_id(unclass(r)), character(1))
  cur <- signer
  for (step in 1:8) {
    if (key_id(cur) %in% root_keys) return(TRUE)
    p <- cert_parts(cur)
    if (identical(p$issuer, p$subject)) return("the certificate does not lead to a trusted authority")
    if (is.na(p$from) || at < p$from || at > p$to) return("a certificate was not valid at the time of the stamp")
    hash <- hash_of(p$sig_alg)
    if (is.null(hash)) return("unknown signature algorithm")
    issuer <- NULL
    for (cand in pool) {
      if (!identical(cert_parts(cand)$subject, p$issuer)) next
      ok <- tryCatch(openssl::signature_verify(p$tbs, p$signature, hash = hash,
                                               pubkey = as.list(read_cert_der(cand))$pubkey),
                     error = function(e) FALSE)
      if (isTRUE(ok)) { issuer <- cand; break }
    }
    if (is.null(issuer)) return("the certificate does not lead to a trusted authority")
    cur <- issuer
  }
  "the certificate chain is too long"
}

# A certificate's subject and public key, which identify an authority even
# when the same authority is certified by another one (cross-signing).
key_id <- function(der) {
  paste(c(as.character(cert_parts(der)$subject),
          as.character(openssl::fingerprint(as.list(read_cert_der(der))$pubkey))), collapse = "")
}

cert_name <- function(der) {
  subject <- as.list(read_cert_der(der))$subject
  cn <- regmatches(subject, regexpr("(?<=CN=)[^,]+", subject, perl = TRUE))
  if (length(cn)) cn else subject
}

# The state of the timestamp of a report: "valid", "changed" (the report was
# edited after the stamp), "invalid", "missing" or "none".
report_timestamp <- function(report) {
  parts <- split_timestamp(report)
  damaged <- list(status = "invalid", why = "the timestamp at the end of the report is damaged")
  if (parts$damaged) return(damaged)
  if (is.null(parts$token)) {
    requested <- any(grepl("^Timestamp: +at the end", readLines(report, warn = FALSE)))
    return(list(status = if (requested) "missing" else "none"))
  }
  ts <- tryCatch(read_timestamp(parts$token), error = function(e) NULL)
  if (is.null(ts)) return(damaged)
  v <- tryCatch(verify_token(ts), error = function(e) list(valid = FALSE, why = damaged$why))
  if (!v$valid) return(list(status = "invalid", why = v$why))
  same <- identical(ts$imprint, as.raw(openssl::sha256(parts$content))) && !parts$added
  list(status = if (same) "valid" else "changed", time = ts$time, service = v$service)
}
