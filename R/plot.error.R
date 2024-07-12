#' plot error
#'
#' Plot error rate for a rfsrc class object.
#'
#' @param o rfsrc object (classication).
#' @param standardize standardize values
#' 
#' @export

plot.error <- function(x, standardize = TRUE, xlab = "Number of Trees",
                       ylab = "Error Rate", title = TRUE, show.plots = TRUE)

{

  ## coherence check
  if (sum(inherits(x, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)'")
  }

  ## block size needs to be 1 for this to be meaningful
  if (x$block.size != 1) {
    x <- predict(x, block.size=1)
  }
  
  ## family specific processing
  if (x$family == "regr" | x$family == "surv") {
    error <- data.frame(trees = 1:x$ntree, error = x$err.rate)
  }
  else if (x$family == "class") {
    error <- data.frame(trees = 1:x$ntree, x$err.rate)
    colnames(error)[-1] <- c("all", levels(x$yvar))
  }
  else if (x$family == "surv-CR") {
    error <- data.frame(trees = 1:x$ntree, x$err.rate)
    colnames(error)[-1] <- x$event.info$event.type
  }
  else if (x$family == "regr+" | x$family == "class+" | x$family == "mix+") {
    error <- data.frame(do.call(cbind,
       lapply(x$yvar.names, function(outcome.target) {
         o <- randomForestSRC:::coerce.multivariate(x, outcome.target)
         err <- cbind(o$err.rate)[, 1]
         if (o$family == "regr") {
           err <- err / var(o$yvar)
         }
         err
       })))
    colnames(error) <- x$yvar.names
    error <- data.frame(trees = 1:x$ntree, error)
  }
  else {
    stop("the supplied example is not a supported family")
  }

  ## long format
  if (!(x$family == "regr" | x$family == "surv")) {
    error <- reshape(error,
                     varying = list(colnames(error)[-1]),
                     v.names = "value", 
                     timevar = "variable", 
                     times = colnames(error)[-1],
                     direction = "long")
    rownames(error) <- NULL
  }
  
  
  # Create ggplot for error rate
  if (x$family == "regr" | x$family == "surv") {
     g <- ggplot(error, aes(x = trees, y = error)) +
       geom_step(color = "#F8766D", linewidth = 1) + theme_minimal() +
       labs(x = xlab, y = ylab) 
  }
  else {
    g <- ggplot(error, aes(x = trees, y = value, color = variable, group = variable)) +
      geom_step(linewidth = 1) + theme_minimal() + labs(x = xlab, y = ylab)
  }
  
  ## add family specific final details
  if (x$family == "regr" | x$family == "surv") {
    # do nothing
  }
  else if (x$family == "class") {
    if (length(sort(unique(x$yvar))) < 10) {
      g <- theme.error.legend(g, 1, 1.5, 1)
    }
    else {
      error <- theme.error.legend(g)
    }
  }
  else if (x$family == "surv-CR") {
    g <- theme.error.legend(g, 1, 2)
    g <- theme.error.legend.adjust(g, error)
  }
  else {
    g <- theme.error.legend(g)
  }

  ## return the ggplot
  if (title) {
    g <- g + labs(title = "Error Rate Over Number of Trees")
  }
  if (show.plots) {
    g
  }
  else {
    invisible(g)
  }

}

