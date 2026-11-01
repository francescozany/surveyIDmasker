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
