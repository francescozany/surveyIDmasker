# surveyIDmasker

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23599338.svg)](https://doi.org/10.5281/zenodo.23599338)
[![R-CMD-check](https://github.com/francescozany/surveyIDmasker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/francescozany/surveyIDmasker/actions/workflows/R-CMD-check.yaml)

Mask participant identifiers in survey exports before sharing them.

**Works with survey data files** in CSV, SPSS `.sav`, Stata `.dta` and Excel
`.xlsx` format: for example Qualtrics exports (extra header rows are kept), Credamo
exports (Chinese column names) and lab data files with one header row.
Tested so far on Qualtrics, Credamo and lab data; support for other survey
tools will be checked as we go.

Prolific, CloudResearch Connect and MTurk IDs, IP addresses, locations and
emails are replaced with `MSK` plus an encrypted code, for example
`MSKBccdxZewIPBZRaFxcnEafypOrWP6dPsTkW2x`. The same value always gets the same code within the file,
so repeat participants can still be spotted. Blank cells become `MSKblank`.

## Install

```r
install.packages("remotes")
remotes::install_github("francescozany/surveyIDmasker")
```

## Use

```r
library(surveyIDmasker)
mask_survey()
```

That's it. You will be asked to:

1. pick your survey file (`.csv`, `.sav`, `.dta` or `.xlsx`). To mask several files
   using the same key, select them together (Cmd-click on a Mac, Ctrl-click
   on Windows);
2. confirm the suggested identifier columns (press Enter), or type `e` to choose them yourself;
3. choose how to mask IP addresses and locations:
   - **fully**, like the other identifiers, or
   - **keeping the country**, and the state for the US: `MSKUS-CT_...` (Connecticut), `MSKIT_...` (Italy).
     The region always ends at `_`.

Changed your mind? Type `q` at any question (or press Esc) to exit.
Nothing is saved until the end.

Files written (the original is never changed):

- `<name>_masked_<date>_<time>.<ext>`: the file to share;
- `<name>_masked_<date>_<time>_report.txt`: what was masked, when, and with
  which key (shown only by the time it was created), plus a sentence ready to
  paste into your methods section or replication README;
- `<name>_KEY_<date>_<time>.txt`: the private key. **Keep it, never share it.**

`<date>_<time>` is when you ran it, e.g. `my_study_masked_2026-10-23_14-53.csv`.

### Several files with the same key

Mask several studies in one run, so the same participant gets the same code
in all of them:

```r
mask_survey(c("study1.csv", "study2.sav", "pilot.xlsx"), columns = "suggested")
mask_survey("my_project_folder", columns = "suggested")   # every survey file in it
```

In the guided mode, select several files at once in the file window
(Cmd-click on a Mac, Ctrl-click on Windows; on a Mac the window may open
behind RStudio). Columns are chosen file by file; how IP addresses and
locations are masked is asked once for all files.

Finer control is in the one-line form: files from different folders, a whole
folder, columns for each file, or different IP/location rules for each file
(e.g. studies with different IRB rules on what can be shared):

```r
mask_survey(c("study1.csv", "other_folder/study2.sav"),
            columns = list(study1.csv = "suggested", study2.sav = c("PROLIFIC_PID", "IPAddress")),
            ip = "full",
            latlon = list(study1.csv = "country_state", study2.sav = "full"))
```

A run with several files writes one masked file next to each original, plus,
in the folder of the first file, one `batch_KEY_<date>_<time>.txt` and one
`batch_masked_<date>_<time>_report.txt`. The report lists every file with its
masked columns and checksum, and says that all were masked with the same key,
identified only by when it was created (the key itself is never shown). The
key file keeps the list of every file masked with it. `reidentify()` also takes
several files at once.

### In one line (scripts, AI agents)

Anything you give as an argument is not asked, so this runs without questions:

```r
list_columns("my_study.sav")     # shows column numbers; * = suggested

mask_survey("my_study.sav",
               columns = c(4, 14, 15, 36),   # numbers, names, or "suggested"
               ip      = "full",             # "full" or "country_state"
               latlon  = "country_state")    # "full" or "country_state"
```

After every run, the console prints the one-line command that repeats it.

## Good to know

- **The key file.** Codes are the identifiers encrypted with a private key,
  saved as `<name>_KEY_<date>_<time>.txt` next to your original file. Keep it with the
  original data and **never share it**. Without it, nobody can turn the
  codes back into identifiers. With it, you can:
  ```r
  reidentify("my_study_masked_2026-10-23_14-53.csv",
             key_file = "my_study_KEY_2026-10-23_14-53.txt")
  ```
- **Same codes across studies**: reuse the key with
  `key_file = "my_study_KEY_2026-10-23_14-53.txt"`. The same participant then gets the same
  code in every file masked with that key.
- **Spotting repeat participants**: the same platform ID code
  (`workerId`, `PROLIFIC_PID`, `participantId`) means the same person. Equal
  IP or location codes only mean the same network or the same estimated
  place (survey platforms only estimate locations), so treat them as supporting
  evidence, not proof.
- **Unknown places**: a location that can't be matched to a country gets a
  plain `MSK...` code.
- **IP addresses with country/state** need the free DB-IP "IP to City Lite"
  CSV: download it from <https://db-ip.com/db/lite.php>, unzip it, and add
  `ip_db = "path/to/dbip-city-lite.csv"`. Reading it takes about 20 seconds.
  IP geolocation by DB-IP (db-ip.com), CC BY 4.0: please credit it if you use it.
- **Free text**: other columns are checked for things that look like IDs,
  emails, IP addresses or phone numbers (participants sometimes type their ID
  in comments). Use `mask_text = TRUE` to replace them as well.
- **Location details** such as `zip`, `city` or `region` columns (added by
  some geolocation steps) are suggested too: a zip code can identify a person.
- **CSV files must be UTF-8**, as Qualtrics and most tools save them. Other
  encodings (e.g. CSVs saved by Excel in China) are refused with an error:
  save the file again as "CSV UTF-8" first.
- **Excel files: only the first sheet** is read and written. If your file has
  other sheets, they are not in the masked file: keep the data on one sheet,
  or save each sheet as its own file.
- **Before sharing**, `check_survey("my_study_masked_....csv")` lists anything
  that still looks identifying.
- Your survey responses never leave your computer. How the codes are made is
  explained in [CODES.md](https://github.com/francescozany/surveyIDmasker/blob/main/CODES.md).

## Cite

> Zanibellato, F. (2026). *surveyIDmasker: Mask participant identifiers in
> survey exports* (Version 0.3.1). Zenodo.
> https://doi.org/10.5281/zenodo.23599338
