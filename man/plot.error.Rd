\name{plot.error}
\alias{plot.error}
\title{Plot error rate for rfsrc object}
\usage{\method{plot}{error}(o, standardize = TRUE, xlab = "Number of Trees",
       ylab = "Error Rate", title = TRUE, , show.plots = TRUE)}
\arguments{
\item{o}{rfsrc object.}
\item{standardize}{Use standardized VIMP?}
\item{xlab}{Label for x-axis.}
\item{ylab}{Label for y-axis.}
\item{title}{Include a title?}
\item{show.plots}{Should plots be displayed?}
}
\description{
Plots standardized error rate for an rfsrc object.
}
\examples{
## multiclass
library(mlbench)
data(Glass, package = "mlbench")
o <- rfsrc(Type ~ ., Glass)
plot.error(o)
}
