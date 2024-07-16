#' Helper Function
#'
#' This function helps the main function.
#'
#' @export

run_random_cr_forest <- function(formula, data, ntree = 100, nodesize = 15, ...) {

   ## collect together hidden options
  dots <- list(...)
  add.ci <- get.add.ci(dots)
  show.plots <- get.show.plots(dots)
  verbose <- get.verbose(dots)
  dots$show.plots <- NULL
  
  # Run a random cr forest on the dataset with block.size = 1
  rf_model <- rfsrc(formula, data = data, ntree = ntree, nodesize = nodesize)
  
  # Print a summary of the model
  print(rf_model)
  
  # Tune the nodesize parameter
  tune_result <- tune.nodesize(formula, data = data, ntreeTry = 100, sampsize = mysampsize, trace = verbose)
  
  # Fit the model again using the optimal nodesize value and block.size = 1
  optimal_nodesize <- tune_result$nsize.opt
  rf_model_tuned <- rfsrc(formula, data = data, ntree = ntree,
                          nodesize = optimal_nodesize, importance = "permute", block.size = 1)
  
  # Print a summary of the tuned model
  print(rf_model_tuned)

  # Create a ggplot for the tuned model
  nodesize_plot_tuned <- plot.tune.nodesize(tune_result, .9)
  
  # Create ggplot for error rate
  error_plot_tuned <- plot.error(rf_model_tuned)
  
  # Extract minimal depth
  md_tuned <- max.subtree(rf_model_tuned)$order
  md_tuned_df <- data.frame(variable = rownames(md_tuned), depth = md_tuned[,1])
  
  # Create ggplot for minimal depth of tuned model
  minimal_depth_plot_tuned <- ggplot(md_tuned_df, aes(x = reorder(md_tuned_df$variable, md_tuned_df$depth), y = md_tuned_df$depth)) +
    geom_bar(stat = "identity", fill = "#F8766D") +
    coord_flip() +
    labs(title = "Minimal Depth", x = "Variable", y = "Minimal Depth") + theme_minimal()
  minimal_depth_plot_tuned <- theme.md(minimal_depth_plot_tuned, .9, .9)

  # Create ggplot for event probabilities
  event_plot_tuned <- plot.cif(rf_model_tuned, show.plots = FALSE)
  
  # Create ggplot for cause-specific variable importance
  if (!add.ci) {
    importance_plot_tuned <- plot.vimp(rf_model_tuned, show.plots = FALSE)
  }
  else {
    importance_plot_tuned <- do.call("plot.vimp.ci", c(list(x = rf_model_tuned, show.plots = FALSE), dots))
  }

  # Combine plots
  if (length(rf_model_tuned$event.info$event.type) < 5) {
    first_row <-  error_plot_tuned + nodesize_plot_tuned + minimal_depth_plot_tuned +
     event_plot_tuned + plot_layout(ncol = 4)
    second_row <- importance_plot_tuned
    combined_plots <- first_row / second_row
  }
  else {
    first_row <-  error_plot_tuned + nodesize_plot_tuned + minimal_depth_plot_tuned +
      event_plot_tuned + plot_layout(ncol = 4)
    second_row <- importance_plot_tuned
    combined_plots <- first_row / second_row + plot_layout(heights = c(1, 3))
  }

  # Return the combined all plots
  if (show.plots) {
    plot(combined_plots)
  }
  else {
    invisible(combined_plots)
  }

}

run.random.cr.forest <- run_random_cr_forest

