# qualtricsmasker

Mask participant identifiers in Qualtrics exports before sharing them.

Prolific, CloudResearch Connect and MTurk IDs, IP addresses, locations and
emails are replaced with `MSK` plus a random 10-character code, for example
`MSK4fQz81LbXa`. The same value always gets the same code within the file,
so repeat participants can still be spotted. Blank cells become `MSKblank`.

## Install

```r
install.packages("remotes")
remotes::install_github("francescozany/qualtricsmasker")

# Only needed to keep country/state in masked locations:
install.packages(c("maps", "countrycode"))
```

## Use

```r
library(qualtricsmasker)
mask_qualtrics()
```

That's it. You will be asked to:

1. pick your Qualtrics export (`.csv`, `.sav` or `.xlsx`);
2. confirm the suggested identifier columns (press Enter), or type `e` to choose them yourself;
3. choose how to mask IP addresses and locations:
   - **fully**, like the other identifiers, or
   - **keeping the country**, and the state for the US: `MSKUSCT...` (Connecticut), `MSKIT...` (Italy).

Changed your mind? Type `q` at any question (or press Esc) to exit.
Nothing is saved until the end.

Two files are written next to the original, which is never changed:

- `<name>_masked.<ext>`: the file to share;
- `<name>_masked_report.txt`: what was masked and when.

### In one line (scripts, AI agents)

Anything you give as an argument is not asked, so this runs without questions:

```r
list_columns("my_study.sav")     # shows column numbers; * = suggested

mask_qualtrics("my_study.sav",
               columns = c(4, 14, 15, 36),   # numbers, names, or "suggested"
               ip      = "full",             # "full" or "country_state"
               latlon  = "country_state")    # "full" or "country_state"
```

After every run, the console prints the one-line command that repeats it.

## Good to know

- **Codes are random and cannot be reversed.** No key or lookup table is saved.
- **Same codes across studies**: use `seed = "a long private phrase"`. The
  same ID then gets the same code in every file you mask with that seed.
  Keep the seed private, and don't use short seeds like `123`: they can be guessed.
- **Unknown places**: a location that can't be matched to a country gets a
  plain `MSK...` code.
- **IP addresses with country/state** need the free DB-IP "IP to City Lite"
  CSV: download it from <https://db-ip.com/db/lite.php>, unzip it, and add
  `ip_db = "path/to/dbip-city-lite.csv"`. Reading it takes about 20 seconds.
  IP geolocation by DB-IP (db-ip.com), CC BY 4.0: please credit it if you use it.
- **Free text**: other columns are checked for things that look like IDs,
  emails or IP addresses (participants sometimes type their ID in comments).
  Use `mask_text = TRUE` to replace them as well.
- Your survey responses never leave your computer.
