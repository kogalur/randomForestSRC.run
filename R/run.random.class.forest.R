#' Helper Function
#'
#' This function helps the main function.
#'
#' @export

run_random_class_forest <- function(formula, data, ntree = 100, nodesize = 1, ...) {

  ## collect together hidden options
  dots <- list(...)
  add.ci <- get.add.ci(dots)
  show.plots <- get.show.plots(dots)
  verbose <- get.verbose(dots)
  dots$show.plots <- NULL
  
  # Run a random forest on the dataset with block.size = 1
  rf_model <- rfsrc(formula, data = data, ntree = ntree, nodesize = nodesize)
  
  # Print a summary of the model
  print(rf_model)

  ## Determine the number of classes
  J <- length(sort(unique(rf_model$yvar)))
    
  # Tune the nodesize parameter
  tune_result <- tune.nodesize(formula, data = data, ntreeTry = 100, sampsize = mysampsize, trace = verbose)

  # Fit the model again using the optimal nodesize value and block.size = 1
  optimal_nodesize <- tune_result$nsize.opt
  rf_model_tuned <- rfsrc(formula = formula, data = data, ntree = ntree,
                          nodesize = optimal_nodesize, importance = "permute", block.size = 1)
  
  # Print a summary of the tuned model
  print(rf_model_tuned)

  # Create a ggplot for the tuned model
  nodesize_plot_tuned <- plot.tune.nodesize(tune_result, .9)
  
  # Create ggplot for error rate
  error_plot_tuned <- plot.error(rf_model_tuned)

  # Obtain the ROC-AUC and PR-AUC
  if (J < 5) {
    roc_pr_tuned <- plot.auc.pr(rf_model_tuned, 0.45, 0.45)
  }
  else {
    roc_pr_tuned <- plot.auc.pr(rf_model_tuned, 0.45, 0.3)
  }

  # Create ggplot for variable importance
  if (!add.ci) {
    importance_plot_tuned <- plot.vimp(rf_model_tuned, show.plots = FALSE)
  }
  else {
    importance_plot_tuned <- do.call("plot.vimp.ci", c(list(x = rf_model_tuned, show.plots = FALSE), dots))
  }
  
  # Combine plots
  if (J < 5) {
    first_row <-  error_plot_tuned + nodesize_plot_tuned + roc_pr_tuned + plot_layout(ncol = 3)
    second_row <- importance_plot_tuned
    combined_plots <- first_row / second_row
  }
  else {
    first_row <-  error_plot_tuned + nodesize_plot_tuned + roc_pr_tuned + plot_layout(ncol = 3)
    second_row <- importance_plot_tuned
    combined_plots <- first_row / second_row + plot_layout(heights = c(1, 1))
  }

  # Return the combined all plots
  if (show.plots) {
    plot(combined_plots)
  }
  else {
    invisible(combined_plots)
  }

}

run.random.class.forest <- run_random_class_forest
