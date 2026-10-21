state_abb <- function(name) {
  abb <- c(datasets::state.abb, "DC")
  full <- c(tolower(datasets::state.name), "district of columbia")
  abb[match(tolower(trimws(name)), full)]
}

# Region code for each coordinate: "IT", "FR", ... and "USCT", "USNY", ... for
# the United States. NA when the point falls outside all mapped countries.
latlon_region <- function(lat, lon) {
  for (pkg in c("maps", "countrycode")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("latlon = \"country_state\" needs the '", pkg, "' package: ",
           "install.packages(\"", pkg, "\")")
    }
  }
  lat <- suppressWarnings(as.numeric(lat))
  lon <- suppressWarnings(as.numeric(lon))
  out <- rep(NA_character_, length(lat))
  ok <- !is.na(lat) & !is.na(lon)
  if (!any(ok)) return(out)

  world <- maps::map.where("world", lon[ok], lat[ok])
  country <- sub(":.*", "", world)
  iso <- suppressWarnings(countrycode::countrycode(country, "country.name", "iso2c"))

  state <- state_abb(sub(":.*", "", maps::map.where("state", lon[ok], lat[ok])))
  # Alaska and Hawaii are not in the "state" map but are subregions of "USA".
  sub_state <- state_abb(sub("^[^:]*:?", "", world))
  state[is.na(state)] <- sub_state[is.na(state)]

  us <- !is.na(iso) & iso == "US" & !is.na(state)
  iso[us] <- paste0("US", state[us])
  out[ok] <- iso
  out
}

ip_to_number <- function(ip) {
  ip <- trimws(ip)
  ok <- !is.na(ip) & grepl("^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$", ip)
  out <- rep(NA_real_, length(ip))
  if (!any(ok)) return(out)
  p <- matrix(as.numeric(unlist(strsplit(ip[ok], ".", fixed = TRUE))), ncol = 4, byrow = TRUE)
  valid <- rowSums(p > 255) == 0
  out[ok][valid] <- (p %*% 256^(3:0))[valid]
  out
}

# Reads a DB-IP Lite CSV (https://db-ip.com/db/lite.php). Both the
# "IP to City" file (ip_start, ip_end, continent, country, stateprov, ...) and
# the "IP to Country" file (ip_start, ip_end, country) work. IPv6 rows are
# skipped.
read_ip_db <- function(path) {
  first <- readr::read_csv(path, col_names = FALSE, n_max = 1, progress = FALSE,
                           col_types = readr::cols(.default = "c"))
  city <- ncol(first) >= 5
  keep <- if (city) c(1, 2, 4, 5) else 1:3
  db <- readr::read_csv(path, col_names = FALSE, progress = FALSE,
                        col_select = paste0("X", keep),
                        col_types = readr::cols(.default = "c"))
  db <- db[!grepl(":", db[[1]], fixed = TRUE), ]
  db <- data.frame(start = ip_to_number(db[[1]]), end = ip_to_number(db[[2]]),
                   country = db[[3]],
                   state = if (city) db[[4]] else NA_character_,
                   stringsAsFactors = FALSE)
  db[order(db$start), ]
}

ip_region <- function(ip, db) {
  n <- ip_to_number(ip)
  out <- rep(NA_character_, length(ip))
  i <- findInterval(n, db$start)
  hit <- !is.na(n) & i > 0
  hit[hit] <- n[hit] <= db$end[i[hit]]
  out[hit] <- db$country[i[hit]]
  out[out %in% "ZZ"] <- NA  # DB-IP code for unknown
  st <- state_abb(db$state[i[hit]])
  us <- out[hit] == "US" & !is.na(st)
  out[hit][us] <- paste0("US", st[us])
  out
}
