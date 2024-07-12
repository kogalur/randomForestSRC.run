#' Helper Function
#'
#' This function helps the main function.
#'
#' @export

run_random_survival_forest <- function(formula, data, ntree = 100, nodesize = 15,
                    loo = 25, plot.survival.curves = TRUE, ...)
{

  ## collect together hidden options
  dots <- list(...)
  add.ci <- get.add.ci(dots)
  show.plots <- get.show.plots(dots)
  verbose <- get.verbose(dots)
  dots$show.plots <- NULL

  # Run a random survival forest on the dataset with block.size = 1
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
  nodesize_plot_tuned <- plot.tune.nodesize(tune_result)
  
  # Create ggplot for error rate of tuned model
  error_plot_tuned <- plot.error(rf_model_tuned)
  
  # Create ggplot for variable importance
  if (!add.ci) {
    importance_plot_tuned <- plot.vimp(rf_model_tuned, show.plots = FALSE)
  }
  else {
    importance_plot_tuned <- do.call("plot.vimp.ci", c(list(x = rf_model_tuned, show.plots = FALSE), dots))
  }
     
  # Calculate Brier score and time-dependent AUC using the riskRegression Score function
  # Run a simplified forest to speed up calculations - bind nodesize for riskRegression
  rfsrc.obj <- do.call("rfsrc", c(list(formula = formula, data = data),
                 list(perf.type = "none", ntree = ntree, nodesize = optimal_nodesize)))
  combined_plot <- plot.brier.auc(rfsrc.obj, loo = loo, show.plots = FALSE)

  
  # Plot survival curves if the option is TRUE
  if (plot.survival.curves) {

    # Pull time and oob survival
    time <- rf_model_tuned$time.interest
    surv <- rf_model_tuned$survival.oob

    # Prepare data frame for survival curves plot
    surv_df <- data.frame(time = rep(time, nrow(surv)), 
                          survival = as.vector(t(surv)),
                          id = rep(1:nrow(surv), each = length(time)))

    # Calculate the average survival curve
    average_survival <- colMeans(surv, na.rm = TRUE)

    # Create a data frame for the average survival curve
    average_surv_df <- data.frame(time = time, survival = average_survival, id = nrow(surv)+1)

    # Create ggplot for survival curves
    survival_plot <- ggplot(surv_df, aes(x = time, y = survival, group = id)) +
      geom_step(alpha = 1, linewidth = 0.25, color = gray(.55)) +
      geom_step(data = average_surv_df, aes(x = time, y = survival, group = id), color = "#F8766D", size = 1) +
      labs(title = "Survival Curves for All Data Points", x = "Time", y = "Survival Probability") +
      theme_minimal()

  }

  # Combine all the plots including survival curves
  if (plot.survival.curves) {
    top <- error_plot_tuned + nodesize_plot_tuned + importance_plot_tuned
    combined_all_plots <- top / (combined_plot | survival_plot)
  }
  else {
    top <- error_plot_tuned + nodesize_plot_tuned
    combined_all_plots <- top / (importance_plot_tuned | combined_plot)
  }


  # Return the combined all plots
  if (show.plots) {
    plot(combined_all_plots)
  }
  else {
    invisible(combined_all_plots)
  }

}

# Example usage:
#example.survival <- function() {
  
#  data(pbc, package = "randomForestSRC")
#  run_random_survival_forest(Surv(days, status) ~ ., pbc)

#  data(veteran, package = "randomForestSRC")
#  run_random_survival_forest(Surv(time, status) ~ ., veteran)

#}

run.random.survival.forest <- run_random_survival_forest

