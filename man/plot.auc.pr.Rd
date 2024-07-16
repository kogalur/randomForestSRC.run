\name{plot.auc.pr}
\alias{plot.auc.pr}
\title{Helper Function}
\usage{\method{plot}{auc.pr}(x, multiplier1 = 0.5, multiplier2 = 0.5, show.plots = TRUE, ...)}
\arguments{
\item{x}{rfsrc object (classification).}
\item{multiplier1}{Scalar for plots.}
\item{multiplier2}{Scalar for plots.}
\item{show.plots}{Should plots be displayed?}
\item{...}{Additional options.}
}
\description{
AUC and pr-AUC plots for a rfsrc (classification) object.
}
\examples{
## multiclass
library(mlbench)
data(Glass, package = "mlbench")
o <- rfsrc(Type ~ ., Glass)
plot.auc.pr(o)
}
