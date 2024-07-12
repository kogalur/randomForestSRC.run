#' run rfsrc
#' 
#' Runs rfsrc on specified data and performs a detailed
#' analysis including tuning the forests and plotting various
#' quantities such as performance metrics and variable importance
#' 
#' @export


run.rfsrc <- function(formula, data, ntree = 100, nodesize = NULL,
                      loo = 25, add.ci = TRUE, show.plots = TRUE, ...) {


  suppressMessages(suppressWarnings({
    
    # used stumped tree to quickly determine family
    stump <- rfsrc(formula, data, mtry = 1, splitrule = "random", nodedepth = 0, 
             perf.type = "none", save.memory = TRUE, ntree = 1)
    family <- stump$family

    ## append additional hidden options
    dots <- list(...)
    dots$add.ci <- add.ci
    dots$show.plots <- show.plots

     # switch based on family
    if (family == "regr") {
      do.call("run_random_regression_forest", c(list(formula = formula,
                 data = data, ntree = ntree, nodesize = nodesize), dots))
    }
    else if (family == "class") {
      do.call("run_random_class_forest", c(list(formula = formula,
                 data = data, ntree = ntree, nodesize = nodesize), dots))
    }
    else if (family == "surv") {
      do.call("run_random_survival_forest", c(list(formula = formula,
                 data = data, ntree = ntree, nodesize = nodesize, loo = loo), dots))
    }
    else if (family == "surv-CR") {
     do.call("run_random_cr_forest", c(list(formula = formula,
                 data = data, ntree = ntree, nodesize = nodesize), dots))
    }
    else if (family == "regr+" | family == "class+" | family == "mix+") {
     do.call("run_random_mv_forest", c(list(formula = formula,
                 data = data, ntree = ntree, nodesize = nodesize), dots))
    }
    else {
      stop("the supplied example is not a supported family")
    }


 }))
  
}


