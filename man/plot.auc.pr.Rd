\name{plot.auc.pr}
\alias{plot.auc.pr}
\title{Helper Function}
\usage{\method{plot}{auc.pr}(o, multiplier1 = 0.5, multiplier2 = 0.5, show.plots = TRUE)}
\arguments{
\item{o}{rfsrc object (classification).}
\item{multiplier1}{Scalar for plots.}
\item{multiplier2}{Scalar for plots.}
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
