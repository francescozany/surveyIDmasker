# surveyIDmasker 0.3.2

* Console output follows CRAN's guidelines. What `mask_survey()` and
  `reidentify()` do is shown as messages, which `quiet = TRUE` or
  `suppressMessages()` silence; only the questions of the guided mode are
  printed. `verify_report()`, `check_survey()` and `list_columns()` return
  their results, which print as before when shown (`x <- verify_report(f)`
  shows nothing; `x` shows the summary; `as.data.frame(x)` gives the plain
  table). `quiet = TRUE` now also silences the notes that were shown before
  even with it (e.g. "Masking the suggested columns").

* `mask_survey(timestamp = TRUE)` gets an independent timestamp of the report
  from a public timestamping service (RFC 3161; Sectigo, or DigiCert if
  Sectigo does not answer). Only the report's SHA-256 fingerprint is sent.
  The timestamp is added at the end of the report, so there is no extra file
  to share. If no service answers, everything else is saved and the report
  says no timestamp was obtained.
* New `verify_report()` checks that masked files still match their report,
  that a timestamped report has not changed since it was timestamped, and
  lists the masked columns of each file. It works offline.
* In the guided mode, identifiers found inside other columns (e.g. a worker
  ID typed into a comment) are now shown before anything is saved, and you
  are asked whether to replace them with their codes. Before, they were only
  mentioned after the files were saved. One-line calls are unchanged: they
  are replaced only with `mask_text = TRUE`.
* Identifiers found inside other columns are now listed with the rows they
  are in (numbered as in the file, below its header rows), on screen and in
  the report. `scan_text()` returns them in a new `rows` column.
* Excel files with several sheets: every sheet is masked, using the same key,
  and the masked file keeps all the sheets. The report has a section for each
  sheet, and `verify_report()`, `check_survey()` and `reidentify()` read every
  sheet. Before, only the first sheet was masked and the others were left out
  of the masked file. `read_survey()` and `list_columns()` get a `sheet`
  argument.
* `verify_report()` recognizes a report by its content, so a renamed copy
  (e.g. `..._report copy.txt`) works too. A masked file that was only renamed
  is recognized by its content as well, and shown as "yes, only the file name
  changed" (new columns `listed_as` and `renamed`).
* Columns that are already masked are no longer suggested, so masking a
  masked file again stops with "This file is already masked" instead of
  masking the codes a second time.
* The end of a random ID used as a file name (e.g.
  `1ABC23D4-56E7-8F90-A123-A4BC56789012.jpg`) is no longer taken for an MTurk
  worker ID.
* `reidentify()` restores codes that were put next to a dash inside text.
* On Windows, the repeat command printed at the end of `mask_survey()` gave
  an error when pasted, because of the backslashes in the file paths. Paths
  are now written with forward slashes, and a file can be named with either
  kind of slash in `columns`, `ip` and `latlon`.
* The help page of `mask_survey()` no longer says the guided mode adds more
  files after the first: several files are selected together in the file
  window.

# surveyIDmasker 0.3.1

* First version submitted to CRAN.
* `mask_survey()`, `reidentify()` and `check_survey()` gain `quiet = TRUE` to
  print nothing.
* Every help page has an example that runs, using a small made-up survey that
  comes with the package (`system.file("extdata", "example_survey.csv",
  package = "surveyIDmasker")`).

# surveyIDmasker 0.3.0

* Several files can be masked in one run using the same key. In the guided
  mode, select them together in the file window (macOS and Windows; elsewhere
  R's one-file window is used). In one line, give a vector of files or a
  folder. The same identifier gets the same code in every file, and one
  combined report lists every file, its masked columns and checksum.
* In one line, columns and IP/location rules can be set for each file with a
  list named by file (e.g. studies with different IRB rules on what can be
  shared). The guided mode asks once for all files. The report gives the rule
  for each file when they differ.
* Reports say which key was used by the time it was created (the key itself
  is never shown), and the key file keeps the list of every file masked with
  it.
* Identifiers from all files being masked are looked for in the free text of
  each file.
* Codes that keep a region now separate it from the code with `_`, and US
  states are written the ISO 3166-2 way: `MSKIT_...`, `MSKUS-CT_...`,
  `MSKCN_...`. Before, the region ran straight into the code (`MSKUSCT...`),
  so it was not always clear where it ended.
* Stata `.dta` files are supported.
* `check_survey()` lists what still looks identifying in a file, e.g. a
  masked file before sharing it.
* Location and contact columns such as `zip`, `city`, `region`, `areacode`
  and `phone` are suggested, and phone numbers are looked for in free text.
* The report ends with a sentence ready to paste into a methods section or a
  replication README, followed by the reference.
* CSV files must be UTF-8; other encodings are refused with an explanation,
  instead of turning non-English text into garbage.
* IDs stored as numbers (in SPSS, Stata or Excel) are written in full before
  masking, so `100000` gets the same code as the text `"100000"` in a CSV
  (before, it was masked as `"1e+05"`).
* Much faster on large files: a 50,000-row survey is masked in about 20
  seconds and re-identified in about 15 (the codes themselves are unchanged).
* An empty file gives a clear error.
* `maps` and `countrycode` are now installed with the package, so keeping the
  country/state of locations works without installing anything else.
* `reidentify()` takes several files (or a folder) at once, and a wrong key
  now says when it was created.

# surveyIDmasker 0.2.0

* Works with survey data files in general, not only Qualtrics exports:
  tested on Qualtrics, Credamo and lab data files.
* Credamo exports: SPSS files whose variable names contain Chinese
  punctuation can now be written, and the Credamo user ID (`用户ID`),
  longitude/latitude (`经度`/`纬度`), province (`省份`) and city (`城市`)
  columns are suggested. Coordinates are treated as a pair, so
  `latlon = "country_state"` works.
* Columns named just `id` (any case) are suggested.
* Coordinates that fall just off the coastline of the world map now get the
  country of the nearest land within about 50 km.
* `read_qualtrics()` is now `read_survey()`.
* Fixed: masking identifiers inside free text no longer turns empty cells
  into the text "NA" (seen in Excel files).

# surveyIDmasker 0.1.0

* First release.
