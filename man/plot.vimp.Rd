\name{plot.vimp}
\alias{plot.vimp}
\title{Plot variable importance (VIMP) for rfsrc object.}
\usage{\method{plot}{vimp}(x, standardize = TRUE, trim = 0, cex = 1,
          xlab = "Importance", ylab = "", title = TRUE, show.plots = TRUE)}
\arguments{
\item{x}{rfsrc object (classification).}
\item{standardize}{Use standardized VIMP?}
\item{trim}{Trim value for VIMP to reduce clutter for multivariate families.}
\item{cex}{Size of text used for multivariate plots, used to reduce clutter.}
\item{xlab}{Label for x-axis.}
\item{ylab}{Label for y-axis.}
\item{title}{Include a title?}
}
\description{

Plots VIMP for rfsrc object.

If users plan to make repeated calls then it is advantageous to
pre-calculate VIMP.  See example below.

For surv-CR families if no VIMP is found in the training forest then the
default action is to run event-specific forests and obtain VIMP from
them. See example below.


}
\examples{
## multiclass
library(mlbench)
data(Glass, package = "mlbench")
o <- rfsrc(Type ~ ., Glass, importance = "permute")
plot.vimp(o)


## cr example without pre-calculated VIMP
## event-specific VIMP will be calculated
data(follic, package = "randomForestSRC")
o <- rfsrc(Surv(time, status) ~ ., follic, ntree = 100)
plot.vimp(o)

## cr example with pre-calculated VIMP
data(follic, package = "randomForestSRC")
o <- rfsrc(Surv(time, status) ~ ., follic, ntree = 100, importance = "permute")
plot.vimp(o)

}
