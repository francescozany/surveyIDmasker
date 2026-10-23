# How to cite the package. Add the DOI here once there is one.
citation_text <- function() {
  sprintf("Zanibellato, F. (2026). surveyIDmasker: Mask participant identifiers in survey exports (Version %s).",
          utils::packageVersion("surveyIDmasker"))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("If you use surveyIDmasker, please cite it as:\n  ", citation_text())
}
