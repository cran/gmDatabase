.onAttach <- function(...) {
  pkgname <- "gmDatabase"
  ver <- utils::packageDescription(pkgname)$Version
  packageStartupMessage("gmDatabase: Working with geometallurgical databases,  Version: ", ver)
}