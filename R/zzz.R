#' @import dplyr
NULL

.onLoad <- function(libname, pkgname) {
    library(randomForestSRC, warn.conflicts = FALSE, quietly = TRUE)
}
