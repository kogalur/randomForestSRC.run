.onAttach <- function(libname, pkgname) {
  rrfsrc.version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                            fields="Version")
  packageStartupMessage(paste("\n",
                              pkgname,
                              rrfsrc.version,
                              "\n",
                              "\n",
                              "\n"))
}
