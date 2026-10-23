# How to cite the package. Add the DOI here once there is one.
citation_text <- function() {
  sprintf("Zanibellato, F. (2026). qualtricsmasker: Mask participant identifiers in Qualtrics exports (Version %s).",
          utils::packageVersion("qualtricsmasker"))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("If you use qualtricsmasker, please cite it as:\n  ", citation_text())
}
