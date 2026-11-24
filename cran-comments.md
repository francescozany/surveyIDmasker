## Resubmission

This is a resubmission. Following the review of 0.3.1, I have:

* Removed console output that could not be easily suppressed. What
  `mask_survey()` and `reidentify()` do is now reported with `message()`, so
  `suppressMessages()` silences it, as does their `quiet` argument.
  `verify_report()`, `check_survey()` and `list_columns()` no longer print:
  they return their results (data frames with a class), which have `print()`
  methods. `cat()` is used only in these print methods and in the questions
  of the guided mode, which runs only in interactive sessions.
* Replaced the commented-out code in the examples with runnable examples;
  the one that needs an internet connection is in `\donttest{}` and works
  without a connection (it then reports that no timestamp was obtained).

As 0.3.1 was not yet on CRAN, this version also includes new features:
independent timestamps of the masking report (`timestamp = TRUE`, using
public RFC 3161 timestamping services, to which only a SHA-256 fingerprint
is sent), the new `verify_report()`, and masking of every sheet of 'Excel'
files. The version number is now 0.3.2.

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
* With `timestamp = TRUE`, a request is sent to a timestamping service
  (Sectigo, or DigiCert if it does not answer), with time-outs. If no service
  answers, the masking is still saved and the user is told. Tests that need
  the internet are skipped on CRAN.
