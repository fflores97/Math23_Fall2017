#Math 23 Script Packages.R
#You need to run this script only once!

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("pracma")
pkgTest("plotrix")

