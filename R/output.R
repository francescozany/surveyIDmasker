# Text written the way cat() writes it, but as a message: suppressMessages()
# (and the `quiet` arguments) silence it. Only the questions of the guided
# mode are written with cat().
say_msg <- function(..., sep = " ") {
  message(paste(unlist(lapply(list(...), as.character)), collapse = sep), appendLF = FALSE)
}

# Results with a text summary (verify_report(), check_survey(),
# list_columns()) are data frames that print as that text. A subset of the
# rows prints as a plain data frame.
with_text <- function(df, text, class) {
  attr(df, "text") <- text
  attr(df, "text_rows") <- nrow(df)
  class(df) <- c(class, "data.frame")
  df
}

print_text <- function(x, ...) {
  text <- attr(x, "text")
  if (is.null(text) || !identical(attr(x, "text_rows"), nrow(x))) {
    print(as.data.frame(strip_text(x)), ...)
  } else {
    cat(text, sep = "\n")
  }
  invisible(x)
}

strip_text <- function(x) {
  attr(x, "text") <- NULL
  attr(x, "text_rows") <- NULL
  class(x) <- "data.frame"
  x
}

#' @export
print.surveyIDmasker_verify <- function(x, ...) print_text(x, ...)

#' @export
print.surveyIDmasker_check <- function(x, ...) print_text(x, ...)

#' @export
print.surveyIDmasker_columns <- function(x, ...) print_text(x, ...)
