# surveyIDmasker

Mask participant identifiers in survey exports (e.g. Qualtrics) before sharing them.

Prolific, CloudResearch Connect and MTurk IDs, IP addresses, locations and
emails are replaced with `MSK` plus an encrypted code, for example
`MSKJQ04U6NUB6dJBSwJy9AaqBx07y`. The same value always gets the same code within the file,
so repeat participants can still be spotted. Blank cells become `MSKblank`.

## Install

```r
install.packages("remotes")
remotes::install_github("francescozany/surveyIDmasker")

# Only needed to keep country/state in masked locations:
install.packages(c("maps", "countrycode"))
```

## Use

```r
library(surveyIDmasker)
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

Files written (the original is never changed):

- `<name>_masked.<ext>`: the file to share;
- `<name>_masked_report.txt`: what was masked and when;
- `<name>_KEY.txt`: the private key. **Keep it, never share it.**

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

- **The key file.** Codes are the identifiers encrypted with a private key,
  saved as `<name>_KEY.txt` next to your original file. Keep it with the
  original data and **never share it**. Without it, nobody can turn the
  codes back into identifiers. With it, you can:
  ```r
  reidentify("my_study_masked.csv", key_file = "my_study_KEY.txt")
  ```
- **Same codes across studies**: reuse the key with
  `key_file = "my_study_KEY.txt"`. The same participant then gets the same
  code in every file masked with that key.
- **Spotting repeat participants**: the same platform ID code
  (`workerId`, `PROLIFIC_PID`, `participantId`) means the same person. Equal
  IP or location codes only mean the same network or the same estimated
  place (Qualtrics locations are approximate), so treat them as supporting
  evidence, not proof.
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
