# Column names (lower case, letters and digits only) that usually hold
# identifiers in survey files (Qualtrics, Credamo, Prolific/MTurk/Connect
# data, lab data).
id_column_names <- c(
  # MTurk
  workerid = "MTurk worker ID", assignmentid = "MTurk/Connect assignment ID",
  hitid = "MTurk HIT ID", mturkcode = "completion code",
  # Prolific
  prolificpid = "Prolific ID", prolificid = "Prolific ID",
  studyid = "Prolific study ID", sessionid = "Prolific session ID",
  # CloudResearch Connect
  participantid = "Prolific/Connect participant ID", projectid = "Connect project ID",
  cloudresearchid = "CloudResearch ID", connectid = "Connect ID",
  # Generic
  id = "participant ID", pid = "participant ID", aid = "participant ID", rid = "participant ID",
  randomid = "random ID", completioncode = "completion code",
  # Qualtrics built-in
  ipaddress = "IP address", locationlatitude = "location (latitude)",
  locationlongitude = "location (longitude)", recipientemail = "email",
  recipientfirstname = "name", recipientlastname = "name",
  externalreference = "external reference", externaldatareference = "external reference",
  email = "email", emailaddress = "email",
  # Credamo (Chinese headers). 作答ID is the response ID, like Qualtrics'
  # ResponseId, so it is not suggested.
  "\u7528\u6237id" = "Credamo user ID", userid = "user ID",          # 用户ID
  "\u7ecf\u5ea6" = "location (longitude)", "\u7eac\u5ea6" = "location (latitude)", # 经度, 纬度
  "\u7701\u4efd" = "location (province)", "\u57ce\u5e02" = "location (city)"      # 省份, 城市
)

# Patterns that identify a whole cell as an identifier. Browser versions
# such as "145.0.0.0" are not IP addresses.
id_patterns <- c(
  "Prolific ID" = "^[0-9a-f]{24}$",
  "MTurk worker ID" = "^A(?=[0-9A-Z]*[0-9])[0-9A-Z]{11,20}$",
  "email" = "^[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}$",
  "IP address" = "^(?!.*\\.0\\.0\\.0$)(\\d{1,3}\\.){3}\\d{1,3}$|^[0-9a-fA-F:]*:[0-9a-fA-F:]+$"
)

normalize_name <- function(x) gsub("[^\\p{L}\\p{N}]", "", tolower(x), perl = TRUE)

#' Suggest columns that contain participant identifiers
#'
#' Looks at column names (e.g. `workerId`, `PROLIFIC_PID`, `participantId`,
#' `IPAddress`, `LocationLatitude`, `RecipientEmail`, and the Credamo user
#' ID, longitude, latitude, province and city columns) and at the content of
#' the columns (values that look like Prolific IDs, MTurk IDs, emails or IP
#' addresses). Empty columns have nothing to mask, so they are left out unless
#' `include_empty = TRUE`.
#'
#' @param data A data frame.
#' @param include_empty Also return identifier columns (recognized by name)
#'   that are empty, e.g. `IPAddress` in an anonymized export.
#' @return A data frame with the suggested `column` names, the `reason`, and
#'   whether the column is `empty`.
#' @export
suggest_id_columns <- function(data, include_empty = FALSE) {
  reasons <- vapply(names(data), function(col) {
    x <- as.character(data[[col]])
    x <- x[!is_blank(x)]
    by_name <- id_column_names[normalize_name(col)]
    if (!length(x)) return(if (include_empty) unname(by_name) else NA_character_)
    if (!is.na(by_name)) return(unname(by_name))
    for (type in names(id_patterns)) {
      if (mean(grepl(id_patterns[[type]], trimws(x), perl = TRUE)) >= 0.5) {
        return(paste(type, "(detected from content)"))
      }
    }
    NA_character_
  }, character(1))
  keep <- !is.na(reasons)
  empty <- vapply(data[keep], function(x) all(is_blank(as.character(x))), logical(1))
  data.frame(column = names(data)[keep], reason = unname(reasons[keep]),
             empty = unname(empty), stringsAsFactors = FALSE)
}

is_latitude <- function(col) grepl("latitude|\u7eac\u5ea6", col, ignore.case = TRUE)
is_longitude <- function(col) grepl("longitude|\u7ecf\u5ea6", col, ignore.case = TRUE)
is_ip_column <- function(data, col) {
  if (normalize_name(col) == "ipaddress") return(TRUE)
  x <- trimws(as.character(data[[col]]))
  x <- x[!is_blank(x)]
  length(x) > 0 && mean(grepl(id_patterns[["IP address"]], x, perl = TRUE)) >= 0.5
}
