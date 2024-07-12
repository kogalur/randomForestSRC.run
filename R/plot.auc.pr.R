#' plot auc pr
#'
#' AUC and pr-AUC plots for a rfsrc class object.
#'
#' @param o rfsrc object (classication).
#' @param multiplier1 Scalar for plots
#' @param multiplier2 Scalar for plots
#' 
#' @export

plot.auc.pr <- function(x, multiplier1 = 0.5, multiplier2 = 0.5, show.plots = TRUE) {

  ## coherence check
  if (sum(inherits(x, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)'")
  }
  if (x$family != "class") {
    stop("this function only works for classification families")
  }
  
  # Define the data
  yvar.unq <- sort(unique(x$yvar))
  J <- length(yvar.unq)
  actual <- as.numeric(factor(as.character(x$yvar)))
  predicted <- x$predicted.oob[, yvar.unq, drop=FALSE]
  
  
  # Function to create PR and ROC data for binary classification
  get_pr_data_binary <- function(actual, predicted) {
    scores_class0 <- predicted[, 2]  # Probabilities for class 1
    pr <- suppressMessages(suppressWarnings(PRROC::pr.curve(scores.class0 = scores_class0,
                        weights.class0 = ifelse(actual == 2, 1, 0), curve = TRUE)))
    data.frame(recall = pr$curve[, 1], precision = pr$curve[, 2], auc = pr$auc.integral)
  }

  get_roc_data_binary <- function(actual, predicted) {
    actual_binary <- ifelse(actual == 2, 1, 0)
    roc_obj <- suppressMessages(suppressWarnings(pROC::roc(actual_binary, predicted[, 2])))
    data.frame(fpr = 1 - roc_obj$specificities, tpr = roc_obj$sensitivities, auc = as.numeric(pROC::auc(roc_obj)))
  }

  # Function to create PR and ROC data for multi-class classification
  get_pr_data_multi <- function(actual, predicted, class_label) {
    scores_class0 <- predicted[, class_label]
    scores_class1 <- as.vector(predicted[, -class_label])
    pr <- suppressMessages(suppressWarnings(PRROC::pr.curve(scores.class0 = scores_class0,
                       scores.class1 = scores_class1, curve = TRUE)))
    data.frame(recall = pr$curve[, 1], precision = pr$curve[, 2], class = factor(class_label), auc = pr$auc.integral)
  }

  get_roc_data_multi <- function(actual, predicted, class_label) {
    actual_binary <- ifelse(actual == class_label, 1, 0)
    roc_obj <- suppressMessages(suppressWarnings(pROC::roc(actual_binary, predicted[, class_label])))
    data.frame(fpr = 1 - roc_obj$specificities, tpr = roc_obj$sensitivities, class = factor(class_label), auc = as.numeric(pROC::auc(roc_obj)))
  }

  # Generate PR and ROC data
  if (J == 2) {
    pr_data <- get_pr_data_binary(actual, predicted)
    roc_data <- get_roc_data_binary(actual, predicted)
  } else {
    pr_data_list <- lapply(1:J, function(class_label) get_pr_data_multi(actual, predicted, class_label))
    pr_data <- do.call(rbind, pr_data_list)
    roc_data_list <- lapply(1:J, function(class_label) get_roc_data_multi(actual, predicted, class_label))
    roc_data <- do.call(rbind, roc_data_list)
  }

  # Create custom labels for the legend
  pr_legend_labels <- paste("AUC =", round(sapply(1:J, function(i) mean(pr_data$auc[pr_data$class == i])), 2))
  roc_legend_labels <- paste("AUC =", round(sapply(1:J, function(i) mean(roc_data$auc[roc_data$class == i])), 2))

  # Plot PR curves
  if (J == 2) {
    pr_plot <- suppressWarnings({ggplot(pr_data, aes(x = recall, y = precision)) +
      geom_line(color = "blue") +
      geom_area(fill = "grey", alpha = 0.2) +
      ggtitle(paste("Precision-Recall Curve for Binary Classification\nAUC =", round(mean(pr_data$auc), 2))) +
      xlab("Recall") +
      ylab("Precision") +
      theme_minimal() +
      theme(
        text = element_text(size = 18 * multiplier1),
        plot.title = element_text(size = 22 * multiplier1),
        axis.title = element_text(size = 20 * multiplier1),
        axis.text = element_text(size = 18 * multiplier1),
        legend.text = element_text(size = 18 * multiplier2),
        legend.title = element_text(size = 20 * multiplier2),
        axis.text.x = element_text(margin = margin(t = 5 * multiplier1)),
        axis.text.y = element_text(margin = margin(r = 5 * multiplier1)),
        axis.ticks = element_blank()
      )})
  } else {
    pr_plot <- suppressWarnings({ggplot(pr_data, aes(x = recall, y = precision, fill = class)) +
      geom_line() +
      geom_area(alpha = 0.2, position = 'identity') +
      ggtitle("Precision-Recall Curves for Multi-Class Classification") +
      xlab("Recall") +
      ylab("Precision") +
      scale_fill_discrete(name = "Class", labels = pr_legend_labels) +
      theme_minimal() +
      theme(
        text = element_text(size = 18 * multiplier1),
        plot.title = element_text(size = 22 * multiplier1),
        axis.title = element_text(size = 20 * multiplier1),
        axis.text = element_text(size = 18 * multiplier1),
        legend.text = element_text(size = 18 * multiplier2),
        legend.title = element_text(size = 20 * multiplier2),
        axis.text.x = element_text(margin = margin(t = 5 * multiplier1)),
        axis.text.y = element_text(margin = margin(r = 5 * multiplier1)),
        axis.ticks = element_blank()
      )})
    if (J >= 5) {
      pr_plot <-  pr_plot + guides(fill = guide_legend(ncol = 2))   
    }
  }
  
  # Plot ROC curves
  if (J == 2) {
    roc_plot <- suppressWarnings({ggplot(roc_data, aes(x = fpr, y = tpr)) +
      geom_line(color = "blue") +
      geom_area(fill = "grey", alpha = 0.2) +
      geom_abline(linetype = "dashed") +
      ggtitle(paste("ROC Curve for Binary Classification\nAUC =", round(mean(roc_data$auc), 2))) +
      xlab("False Positive Rate (1 - Specificity)") +
      ylab("True Positive Rate (Sensitivity)") +
      theme_minimal() +
      theme(
        text = element_text(size = 18 * multiplier1),
        plot.title = element_text(size = 22 * multiplier1),
        axis.title = element_text(size = 20 * multiplier1),
        axis.text = element_text(size = 18 * multiplier1),
        legend.text = element_text(size = 18 * multiplier2),
        legend.title = element_text(size = 20 * multiplier2),
        axis.text.x = element_text(margin = margin(t = 5 * multiplier1)),
        axis.text.y = element_text(margin = margin(r = 5 * multiplier1)),
        axis.ticks = element_blank()
      )})
  } else {
    roc_plot <- suppressWarnings({ggplot(roc_data, aes(x = fpr, y = tpr, fill = class)) +
      geom_line() +
      geom_area(alpha = 0.2, position = 'identity') +
      geom_abline(linetype = "dashed") +
      ggtitle("ROC Curves for Multi-Class Classification") +
      xlab("False Positive Rate (1 - Specificity)") +
      ylab("True Positive Rate (Sensitivity)") +
      #scale_color_discrete(name = "Class", labels = roc_legend_labels) +
      scale_fill_discrete(name = "Class", labels = roc_legend_labels) +
      theme_minimal() +
      theme(
        text = element_text(size = 18 * multiplier1),
        plot.title = element_text(size = 22 * multiplier1),
        axis.title = element_text(size = 20 * multiplier1),
        axis.text = element_text(size = 18 * multiplier1),
        legend.text = element_text(size = 18 * multiplier2),
        legend.title = element_text(size = 20 * multiplier2),
        axis.text.x = element_text(margin = margin(t = 5 * multiplier1)),
        axis.text.y = element_text(margin = margin(r = 5 * multiplier1)),
        axis.ticks = element_blank()
      )})
    if (J >= 5) {
      roc_plot <-  roc_plot + guides(fill = guide_legend(ncol = 2))   
    }
  }

  # Combine the two plots using patchwork
  combined_plot <- roc_plot + pr_plot + plot_layout(ncol = 1)

  # Plot the combined plot
  if (show.plots) {
    combined_plot
  }
  else {
    invisible(combined_plot)
  }
  
}
