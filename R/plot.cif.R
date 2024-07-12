#' plot cif 
#'
#' @export

plot.cif <- function(x, show.plots = TRUE) {

  ## coherence check
  if (sum(inherits(x, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)'")
  }
  if (x$family != "surv-CR") {
    stop("this function only works for survival-CR families")
  }
  

  ## assemble the event free probabilities
  cif <- apply(x$cif.oob, c(2, 3), mean, na.rm = TRUE)
  colnames(cif) <-  paste0("event.", x$event.info$event.type)
  cr <- data.frame(time = x$time.interest, event.free = 1 - rowSums(cif), cif)
  cr <- reshape(cr,
              varying = list(colnames(cr)[-1]),
              v.names = "value", 
              timevar = "variable", 
              times = colnames(cr)[-1],
              direction = "long")
  cr$variable <- gsub("\\.", " ", cr$variable)  
  g <- ggplot(cr, aes(x = time, y = value, color = variable, group = variable)) +
    geom_step(linewidth = 1) + theme_minimal() +
    labs(x = "Time", y = "Probability")
  g <- theme.cr.legend(g, 1, 2)
  g <- theme.cr.legend.adjust(g, cr)

  ## return the plot
  if (show.plots) {
    g
  }
  else {
    invisible(g)
  }

}
