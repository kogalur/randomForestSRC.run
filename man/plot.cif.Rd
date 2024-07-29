\name{plot.cif}
\alias{plot.cif}
\title{Plot Cumulative Incidence Function}
\usage{\method{plot}{cif}(x, show.plots = TRUE, ...)}
\arguments{
\item{x}{rfsrc object (survival-CR).}
\item{show.plots}{Should plots be displayed?}
\item{...}{Additional options.}
}
\description{
CIF and event free probability curves from surv-CR analysis.
}
\examples{
## competing risk
data(pbc, package = "survival")
pbc$id <- NULL
o <- rfsrc(Surv(time, status) ~ ., data = pbc)
plot.cif(o)

## same example, but using show.plots option
g <- plot.cif(o, show.plots = FALSE)
plot(g)
}
