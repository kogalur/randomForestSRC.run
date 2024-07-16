\name{plot.vimp.ci}
\alias{plot.vimp.ci}
\title{Plot confidence intervals for variable importance (VIMP) for rfsrc object.}
\usage{\method{plot}{vimp.ci}(x, standardize = TRUE,
   alpha = .01, trim = 0, cex = 1, size = 2, width = .5,
   show.plots = TRUE, sorted = TRUE, xlab = "Variable Importance", ...)}
\arguments{
\item{x}{rfsrc object or a rfsrc subsample object.}
\item{standardize}{Use standardized VIMP?}
\item{alpha}{Desired level of significance.}
\item{trim}{Trim value for VIMP.}
\item{cex}{Size of text.}
\item{size}{Size of shape used for median in boxplot.}
\item{width}{Line width used in boxplot.}
\item{show.plots}{Should plots be displayed?}
\item{sorted}{Should variables be sorted in terms of of importance?}
\item{xlab}{Label for x-axis.}
\item{...}{Further arguments passed to the subsample function.}
}
\description{
Uses subsampling to obtain confidence intervals for VIMP for a rfsrc
object and then plots the results.

If users plan to make repeated calls then it is advantageous to
pre-calculate VIMP and/or a subsampling object and pass these to
the function.  See example below.

For surv-CR families the default action is to run event-specific
forests.  This can be over-ridden by passing in an already calculated
subsampling object.

}
\examples{
## default call
o <- rfsrc(Ozone ~ ., airquality)
plot.vimp.ci(o)

## make prior call to importance
o <- rfsrc(Ozone ~ ., airquality, importance = "permute")
plot.vimp.ci(o)

## same as above, but with prior call to subsampling
o <- rfsrc(Ozone ~ ., airquality, importance = "permute")
oo <- subsample(o)
plot.vimp.ci(oo)

## same as above, but without plotting
o <- rfsrc(Ozone ~ ., airquality, importance = "permute")
oo <- subsample(o)
g <- plot.vimp.ci(oo, show.plots = FALSE)
plot(g)

## cr example without pre-calculated subsampling
data(follic, package = "randomForestSRC")
o <- rfsrc(Surv(time, status) ~ ., follic, ntree = 100, importance = "permute")
plot.vimp.ci(o)

## cr example with pre-calculated subsampling
data(follic, package = "randomForestSRC")
o <- rfsrc(Surv(time, status) ~ ., follic, ntree = 100, importance = "permute")
oo <- subsample(o)
plot.vimp.ci(oo)

}
