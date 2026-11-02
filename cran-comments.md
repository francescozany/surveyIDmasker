## Submission

This is the first submission of surveyIDmasker to CRAN.

## Test environments

* local macOS (aarch64), R 4.5.1
* GitHub Actions: Windows (release), macOS (release), Ubuntu (devel,
  release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for the reviewers

* Files are written only where the user points: next to the file being
  masked (the masked copy, a report and the key), or to `output =`. The
  original file is never changed. Examples and tests work in `tempdir()`.
* The guided mode (`mask_survey()` with no arguments) asks questions only in
  interactive sessions; all its choices can be given as arguments.
* In `write_sav_any()`, files whose variable names `haven::write_sav()` rejects
  only because of non-ASCII punctuation (common in exports from the Chinese
  survey platform 'Credamo') are written with haven's own writer function,
  `write_sav_()`, obtained with `utils::getFromNamespace()`. The names are
  valid for SPSS and are kept unchanged. This is used only when the
  exported function refuses the names; all other files go through
  `haven::write_sav()`.
