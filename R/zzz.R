# How to cite the package. The DOI is Zenodo's "all versions" DOI, which
# always resolves to the latest release.
citation_text <- function() {
  sprintf("Zanibellato, F. (2026). surveyIDmasker: Mask participant identifiers in survey exports (Version %s). Zenodo. https://doi.org/10.5281/zenodo.23599338",
          utils::packageVersion("surveyIDmasker"))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("If you use surveyIDmasker, please cite it as:\n  ", citation_text())
}
