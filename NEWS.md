# surveyIDmasker 0.2.0

* Works with survey data files in general, not only Qualtrics exports:
  tested on Qualtrics, Credamo and lab data files.
* Credamo exports: SPSS files whose variable names contain Chinese
  punctuation can now be written, and the Credamo user ID (`用户ID`),
  longitude/latitude (`经度`/`纬度`), province (`省份`) and city (`城市`)
  columns are suggested. Coordinates are treated as a pair, so
  `latlon = "country_state"` works (e.g. `MSKCN...`).
* Columns named just `id` (any case) are suggested.
* Coordinates that fall just off the coastline of the world map now get the
  country of the nearest land within about 50 km.
* `read_qualtrics()` is now `read_survey()`.
* Fixed: masking identifiers inside free text no longer turns empty cells
  into the text "NA" (seen in Excel files).

# surveyIDmasker 0.1.0

* First release.
