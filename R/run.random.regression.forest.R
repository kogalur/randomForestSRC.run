#' Helper Function
#'
#' This function helps the main function.
#'
#' @export

run_random_regression_forest <- function(formula, data, ntree = 100, nodesize = 5,
                      plot_quantreg = TRUE, prbL = 0.25, prbU = 0.75, ...) {

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
  
  # Create ggplot for variable importance of tuned model
  if (!add.ci) {
    importance_plot_tuned <- plot.vimp(rf_model_tuned, show.plots = FALSE)
  }
  else {
    importance_plot_tuned <- do.call("plot.vimp.ci", c(list(x = rf_model_tuned, show.plots = FALSE), dots))
  }
  
  # Plot quantreg if the option is TRUE
  if (plot_quantreg) {

    # Quantreg analysis
    quant_model<- quantreg(formula, data = data, ntree = ntree,
                          nodesize = optimal_nodesize, perf.type = "none")

    ## Set the quantile values
    if (prbL < 0 || prbU > 1) {
      stop("requested probabilities must lie in (0, 1)")
    }
    if (prbL >= prbU) {
      stop("prbL must be less than prbU")
    }
    prbM <- max(prbL, 0.5)
    if (prbM == prbL) {
      prbM <- (prbL + prbU)/2
    }
    
    # Create quantile regression data for plot 
    quant.dat <- get.quantile(quant_model, c(prbL, prbM, prbU), FALSE)
    y.names <- names(quant.dat)[1]
    quant.dat <- quant.dat[[1]]
    y <- quant_model$yvar
    crps.dat <- get.quantile.crps(quant_model)

    # Create quantile regression plot
    jitter.y <- jitter(y, 10)
    quantile_plot <- ggplot() +
      geom_point(aes(x = jitter.y, y = quant.dat[,2]), color = "blue", size = 0.75, shape = 15) +
      geom_errorbar(aes(x = jitter.y, ymin = quant.dat[,1], ymax = quant.dat[,3]), color = "grey") +
      geom_point(aes(x = jitter.y, y = quant.dat[,1]), shape = "-", size = 2) +
      geom_point(aes(x = jitter.y, y = quant.dat[,3]), shape = "-", size = 2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(x = "Observed Values", y = "Prediction Intervals") +
      theme_minimal()

    # Create CRPS plot
    crps_plot <- ggplot(data.frame(x = crps.dat[,1], y = crps.dat[, 2]), aes(x = crps.dat[,1], y = crps.dat[, 2])) +
      geom_line(color = "red", size = 1) +
      theme_minimal() +
      theme(axis.title = element_blank())#, axis.text = element_blank(), axis.ticks = element_blank())

    # Calculate adaptive inset position and size based on the main plot data range
    main_plot_data_range_x <- range(jitter.y)
    main_plot_data_range_y <- range(c(quant.dat[,1], quant.dat[,2], quant.dat[,3]))
    
    inset_margin_x <- diff(main_plot_data_range_x) * 0.05
    inset_margin_y <- diff(main_plot_data_range_y) * 0.05
    
    inset_x <- 0.065
    inset_y <- 0.7
    inset_width <- 0.25
    inset_height <- 0.25
    
    # Quantile plot
    quantile_plot <- ggdraw() +
      draw_plot(quantile_plot) +
      draw_plot(crps_plot, x = inset_x, y = inset_y, width = inset_width, height = inset_height) +
      draw_grob(rectGrob(gp = gpar(lwd = 2, col = "black", fill = NA)), 
                x = inset_x, y = inset_y, width = inset_width, height = inset_height)
  }    

  # Combine all the plots 
  if (plot_quantreg) {
    combined_plots <- (error_plot_tuned + nodesize_plot_tuned + importance_plot_tuned) / quantile_plot
  }
  else {
    combined_plots <- error_plot_tuned + nodesize_plot_tuned + importance_plot_tuned + plot_layout(ncol = 3)
  }
    
  # Return the combined all plots
  if (show.plots) {
    plot(combined_plots)
  }
  else {
    invisible(combined_plots)
  }

}


run.random.regression.forest <- run_random_regression_forest
