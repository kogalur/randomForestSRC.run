#' plot brier auc
#'
#' Brier score and time-dependent AUC for a rfsrc survival object.
#'
#' @param o rfsrc object (classication).
#' @param B Number of cross-validation folds
#' 
#' @export

plot.brier.auc <- function(x, loo = 25, show.plots = TRUE) {

  ## coherence check
  if (sum(inherits(x, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)'")
  }
  if (x$family != "surv") {
    stop("this function only works for survival families")
  }

  
  ## use riskRegression Score to obtain the metrics and their standard error
  metrics <- tryCatch({
    suppressWarnings(Score(list(rfsrc = x),
                   formula = new_formula(f_lhs(formula(x$call)), quote(1)),
                   data = data.frame(x$yvar, x$xvar),
                   times = x$time.interest[-1],
                   split.method = "loob", B = loo, verbose = -1))},
                   error = function(ex) {NULL})
  
  if (!is.null(metrics)) {

    ## Remove Null model information
    metrics$Brier$score <- metrics$Brier$score[metrics$Brier$score$model == "rfsrc",, drop=FALSE]
    metrics$AUC$score <- metrics$AUC$score[metrics$AUC$score$model == "rfsrc",, drop=FALSE]
  
    # Create the Brier score using ggplot2
    brier_plot <- ggplot(metrics$Brier$score, aes(x = times, y = Brier)) +
      geom_line(color = "#F8766D", linewidth = 1) +
      geom_ribbon(aes(ymin = Brier - 1.96 * se, ymax = Brier + 1.96 * se), alpha = 0.2) +
      labs(title = "Brier Score Over Time", x = "Time", y = "Brier Score") +
      theme_minimal()
  
    # Create the time-dependent AUC plot using ggplot2
    auc_plot <- ggplot(metrics$AUC$score, aes(x = times, y = AUC)) +
      geom_line(color = "#F8766D", linewidth = 1) +
      geom_ribbon(aes(ymin = AUC - 1.96 * se, ymax = AUC + 1.96 * se), alpha = 0.2) +
      labs(title = "Time-Dependent AUC", x = "Time", y = "AUC") +
      theme_minimal()
  
    # Combine the Brier score plot and the AUC plot 
    if (show.plots) {
      #plot(arrangeGrob(brier_plot, auc_plot, nrow = 2))
      #brier_plot / auc_plot
      brier_plot + auc_plot + plot_layout(ncol = 1)
    }
    else {
      #arrangeGrob(brier_plot, auc_plot, nrow = 2)
      #invisible(brier_plot / auc_plot)
      invisible(brier_plot + auc_plot + plot_layout(ncol = 1))
    }
  }
  else {
    NULL
  }

}
