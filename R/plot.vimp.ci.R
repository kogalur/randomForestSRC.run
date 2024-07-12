#' plot error
#'
#' Plot error rate for a rfsrc class object.
#'
#' @param o rfsrc object 
#' @param standardize standardize values
#' @param alpha value for ci
#' 
#' @export


plot.vimp.ci <- function(x, standardize = TRUE, alpha = .01,
    trim = 0, cex = 1, size = 2, width = .5,
    show.plots = TRUE, sorted = TRUE, ...) {


  ## coherence check
  if (sum(inherits(x, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2
     & (sum(inherits(x, c("rfsrc", "subsample"), TRUE) == c(1, 4)) != 2))   {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, subsample)'")
  }

  ## purify dots for subsample
  dots <- list(...)
  dots <- dots[names(dots) %in% names(formals(subsample))]

  ## subsample the object?
  if (sum(inherits(x, c("rfsrc", "subsample"), TRUE) == c(1, 4)) != 2) {
    family <- x$family  
    if (family != "surv-CR") {
      x <- do.call("subsample", c(list(obj = x), dots))
    }
    else {
      cr.sub.flag <- TRUE
    }
  }
  ## already subsampled
  else {
    family <- x$rf$family
    if (family == "surv-CR") {#pre-calculated subsampling object for surv-CR
      #stop("for surv-CR families, 'x' must be a '(rfsrc, grow)' object, not a subsampled object")
      cr.sub.flag <- FALSE
    }
  }

  ## trim variables
  if (!(family  == "surv-CR" && cr.sub.flag)) {
    xvar.names <- xvar.vimp.trim(x, trim = trim, standardize = standardize)
  }
  
  ## family specific processing
  if (family == "regr" | family == "surv") {
    g <- mybxp.vimp(plot.subsample(x, xvar.names = xvar.names, alpha = alpha, show.plots = FALSE),
                  size = size, width = width, sorted = sorted)
    g <- theme.vimp.ci(g, cex * 1)
  }
  else if (family == "class") {
    vmp <- x$vmp[[1]]
    J <- ncol(vmp) -1
    g <- lapply(1:ncol(vmp), function(j) {
      gg <- mybxp.vimp(plot.subsample(x, target = j - 1, xvar.names = xvar.names,
                                    alpha = alpha, show.plots = FALSE),
                     size = size, width = width, sorted = sorted,
                     lab = paste("Variable Importance:", colnames(vmp)[j]))
      if (J < 5) {
        theme.vimp.ci(gg, cex * 1)
      }
      else {
        theme.vimp.ci(gg, cex * .6)
      }
    })
    if (J < 5) {
      g <- wrap_plots(g, ncol = length(g))
    }
    else {
      g <- wrap_plots(g, ncol = min(length(g), 4))
    }
  }
  else if (family == "surv-CR") {
    if (cr.sub.flag) {#event-specific forests
      events <- x$event.info$event.type
      g <- lapply(1:length(events), function(j) {
        cat("fitting event specific forest...", j, "\n")
        xx <- do.call("subsample",
                      c(list(obj = rfsrc(formula = new_formula(f_lhs(formula(x$call)), quote(.)),
                                         data = data.frame(x$yvar, x$xvar),
                                         ntree = x$ntree,
                                         splitrule = "logrank",
                                         nodesize = x$nodesize,
                                         importance = "permute",
                                         cause = j)), dots))
        xvar.names <- xvar.vimp.trim(xx, trim = trim, standardize = standardize)
        gg <- mybxp.vimp(plot.subsample(xx, target = j, xvar.names = xvar.names,
                                      alpha = alpha, show.plots = FALSE),
                       size = size, width = width, sorted = sorted,
                       lab = paste("Variable Importance: Event", events[j]))
        theme.vimp.ci(gg, cex * 1, cex * .8)
      })
    }
    else {##over-ridden by users pre-calculated subsampling object
      events <- x$rf$event.info$event.type
      g <- lapply(1:length(events), function(j) {
        gg <- mybxp.vimp(plot.subsample(x, target = j, xvar.names = xvar.names,
                                      alpha = alpha, show.plots = FALSE),
                       size = size, width = width, sorted = sorted,
                       lab = paste("Variable Importance: Event", events[j]))
        theme.vimp.ci(gg, cex * 1, cex * .8)
      })
    }
    if (length(events) < 5) {
      g <- wrap_plots(g, ncol = length(g))
    }
    else {
      g <- wrap_plots(g, ncol = min(length(g), 4))
    }
  }
  else if (family == "regr+" | family == "class+" | family == "mix+") {
    outcomes <- x$rf$yvar.names
    g <- lapply(1:length(outcomes), function(j) {
      gg <- mybxp.vimp(plot.subsample(x, m.target = outcomes[j], xvar.names = xvar.names,
                                    alpha = alpha, show.plots = FALSE),
                     size = size, width = width, sorted = sorted,
                     lab = paste("Variable Importance:",  outcomes[j]))
      theme.vimp.ci(gg, .6 * cex)
    })
    g <- wrap_plots(g, ncol = min(length(g), 4))
  }
  else {
    stop("the supplied example is not a supported family")
  }

  ## return the plot
  if (show.plots) {
    plot(g)
  }
  else {
    invisible(g)
  }
  
}

##================================================================================
##
## plot utilities
##
##================================================================================

# Custom colors
gg.color <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
gg.color2 <- function(j, n=10) {
  gg.color(n)[j]
}


mybxp.vimp <- function(o, sorted = TRUE, size = 2, width = .5, lab = "Variable Importance") {

  # Convert boxplot data to format suitable for geom_boxplot
  df <- data.frame(t(o$stats))
  colnames(df) <- c("lower", "Q1", "median", "Q3", "upper")
  df$color <- factor(ifelse(df$lower <= 0, gg.color2(6), gg.color2(1)))
  df$group <- rownames(df)
  rownames(df) <- NULL

  # Sort?
  if (sorted) {
    # Order the groups by the median value in descending order
    desired_order <- df$group[order(df$median, decreasing = TRUE)]
    # Convert 'group' to a factor with the specified levels
    df$group <- factor(df$group, levels = unique(desired_order))
  }

  # Create a ggplot boxplot using the extracted data  
  ggplot(df, aes(x = group, color = color, fill = color)) +
    geom_boxplot(aes(
      ymin = lower,
      lower = Q1,
      middle = median,
      upper = Q3,
      ymax = upper,
      group = group
    ), stat = "identity", colour = "black", size = width) +
    geom_point(aes(y = median), color = "black", size = size, shape = 23, fill = "yellow") +
    labs(y = lab, x = "") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    scale_color_identity() +
    scale_fill_identity() +
    coord_flip()

}

##================================================================================
##
## other utilities
##
##================================================================================

## declutter vimp plot to specified trim (quantile) value
xvar.vimp.trim <- function(x, trim, standardize = TRUE) {
  vmp <- get.mv.vimp(x$rf, standardize = standardize)
  q <- quantile(vmp, trim)
  pt <- apply(vmp, 1, function(x){all(abs(x) >= q)})
  vmp <- vmp[pt,, drop = FALSE]
  if (nrow(vmp) == 0) {
    stop("trim is set too high, no variables meet that cutoff")
  }
  rownames(vmp)
}
