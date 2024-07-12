\name{run.rfsrc}
\alias{run.rfsrc}
\title{run rfsrc}
\usage{run.rfsrc(formula, data, ntree = 100, nodesize = NULL,
   loo = 25, add.ci = TRUE, show.plots = TRUE, ...)}
\arguments{
\item{formula}{Object of class 'formula' describing the model to fit.}

\item{data}{Data frame containing the y-outcome and x-variables.}

\item{ntree}{Number of trees.}

\item{nodesize}{Minumum size of terminal node.}

\item{loo}{Number of Monte Carlo sets used for the leave-one-out (loo)
  bootstrap used for Brier and time dependent AUC standard errors. Only
  applies to survival.}

\item{add.ci}{Add confidence intervals to VIMP?}

\item{show.plots}{Should plots be displayed?}

\item{...}{Additional options passed to helper functions (alpha, trim,
  cex, size, width, sorted).}
}
\value{
A single plot describing the results of the rfsrc analysis.
}
\description{
Runs rfsrc (rrfsrc) on a user's specified data and performs a detailed
analysis including tuning the forests and plotting various
quantities such as performance metrics and variable importance.  Applies
to regression, classification, survival, competing risk and multivariate
families.  See examples below.
}
\details{
  Output values are family specific.
}
\examples{

## regression
data(BostonHousing, package="mlbench")
run.rfsrc(medv ~ ., BostonHousing)

## two-class
data(breast, package = "randomForestSRC")
breast <- na.omit(breast)
breast$status <- factor(breast$status, labels=c("normal", "recurrent"))
run.rfsrc(status ~ ., breast)

## multiclass
data(Glass, package = "mlbench")
run.rfsrc(Type ~ ., Glass)
  
## survival
data(pbc, package = "randomForestSRC")
run.rfsrc(Surv(days, status) ~ ., data = pbc)

## competing risk
data(pbc, package = "survival")
pbc$id <- NULL
run.rfsrc(Surv(time, status) ~ ., data = pbc)

## multivariate
data(nutrigenomic, package = "randomForestSRC")
f <- get.mv.formula(colnames(nutrigenomic$lipids))
d <- data.frame(do.call(cbind, nutrigenomic))
run.rfsrc(f, d, trim = .5, cex = .6)

## multivariate
ydta <- data.frame(diet = nutrigenomic$diet,
                   genotype = nutrigenomic$genotype,
                   nutrigenomic$lipids)
f <- get.mv.formula(colnames(ydta))
d <- data.frame(ydta, nutrigenomic$genes)
run.rfsrc(f, d, trim = .5, cex = .6)

}