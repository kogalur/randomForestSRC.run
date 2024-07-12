#' plot error
#'
#' Plot error rate for a rfsrc class object.
#'
#' @param o rfsrc object 
#' @param standardize standardize values
#' @param trim used for trimming vimp for multivariate plots
#' @param cex for setting size for some of the plots
#' @param xlab label for x-axis
#' @param ylab label for y-axis
#' @param title display title?
#' 
#' @export

plot.vimp <- function(x, standardize = TRUE, trim = 0, cex = 1,
        xlab = "Importance", ylab = "", title = TRUE, show.plots = TRUE) {

  ## coherence check
  if (sum(inherits(x, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)'")
  }

  ## check that vimp is available
  if (x$family != "surv-CR") {
    if (x$family != "class") {
      vmp <- get.mv.vimp(x, standardize = standardize)
    }
    else {
      vmp <- x$importance
    }
    if (is.null(vmp)) {
      cat("no importance found: calculating it now ...\n")
      if (x$family != "class") {
        vmp <- get.mv.vimp(vimp(x, importance="permute", block.size=1), TRUE)
      }
      else {
        vmp <- vimp(x, importance="permute")$importance
      }
    }
  }
  else {## CR family
    vmp <- get.mv.vimp(x, standardize = standardize)
    cr.cause.flag <- FALSE
    if (is.null(vmp)) {
      cr.cause.flag <- TRUE
    }
  }

  ## trim vimp
  if (!(x$family == "surv-CR" && cr.cause.flag)) {
    vmp <- vimp.trim(vmp, trim = trim, standardize = standardize)
  }
  
  ## family specific processing
  if (x$family == "regr" | x$family == "surv") {
    g  <- mybar.vimp(rownames(vmp), vmp[, 1], xlab, ylab)
    g <- theme.vimp(g, cex * 1)
    if (title) {
      g <- g + labs(title = "Variable Importance")
    }
  }
  else if (x$family == "class") {
    J <- length(sort(unique(x$yvar)))
    g <- lapply(1:ncol(vmp), function(j) {
      gg  <- mybar.vimp(rownames(vmp), vmp[, j], xlab, ylab)
      if (title) {
        gg <- gg + labs(title = paste("Variable Importance:", colnames(vmp)[j])) 
      }
      if (J < 5) {
        theme.vimp(gg, cex * 1)
      }
      else {
        theme.vimp(gg, cex * .6)
      }
    })
    if (J < 5) {
      g <- wrap_plots(g, ncol = length(g))
    }
    else {
      g <- wrap_plots(g, ncol = min(length(g), 4))
    }
  }
  else if (x$family == "surv-CR") {
    events <- x$event.info$event.type
    if (cr.cause.flag) {##cause-specific VIMP will be calculated
      g <- lapply(1:length(events), function(j) {
        cat("fitting event specific forest...", j, "\n")
        imp <- rfsrc(formula = new_formula(f_lhs(formula(x$call)), quote(.)),
                   data = data.frame(x$yvar, x$xvar),
                   ntree = x$ntree,
                   splitrule = "logrank",
                   nodesize = x$nodesize,
                   importance = "permute",
                   cause = j)$importance
        imp <- vimp.trim(imp, trim = trim, standardize = standardize)
        gg  <- mybar.vimp(rownames(imp), imp[, j], xlab, ylab)
        if (title) {
          gg <- gg + labs(title = paste("Variable Importance: Event", events[j]))
        }
      theme.vimp(gg, cex * 1, cex * .8)
      })
    }
    else {##user provided VIMP over-rides the default action
      g <- lapply(1:length(events), function(j) {
        gg  <- mybar.vimp(rownames(vmp), vmp[, j], xlab, ylab)
        if (title) {
          gg <- gg + labs(title = paste("Variable Importance: Event", events[j]))
        }
      theme.vimp(gg, cex * 1, cex * .8)
      })
    }
    if (length(events) < 5) {
      g <- wrap_plots(g, ncol = length(g))
    }
    else {
      g <- wrap_plots(g, ncol = min(length(g), 4))
    }
  }
  else if (x$family == "regr+" | x$family == "class+" | x$family == "mix+") {
    outcomes <- x$yvar.names
    g <- lapply(1:length(outcomes), function(j) {
      gg  <- mybar.vimp(rownames(vmp), vmp[, j], xlab, ylab)
      if (title) {
        gg <- gg + labs(title = outcomes[j])
      }
      theme.vimp(gg, .6 * cex)
    })
    g <- wrap_plots(g, ncol = min(length(g), 4))
  }
  else {
    stop("the supplied example is not a supported family")
  }

  ## return the plot
  if (show.plots) {
    g
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

mybar.vimp <- function(nms, vmp, xlab, ylab) {

  df <- data.frame(variable = nms, importance = vmp)
  df$color <- factor(ifelse(df$importance > 0, gg.color2(6), gg.color2(1)))
  ggplot(df, aes(x = reorder(variable, importance), y = importance, fill = color)) +
    geom_bar(stat = "identity") + coord_flip() + theme_minimal() +
    labs(x = ylab, y = xlab) +
    theme(legend.position = "none")
  
}

##================================================================================
##
## other utilities
##
##================================================================================

## declutter vimp plot to specified trim (quantile) value
vimp.trim <- function(vmp, trim, standardize = TRUE) {
  q <- quantile(vmp, trim)
  pt <- apply(vmp, 1, function(x){all(abs(x) >= q)})
  vmp <- vmp[pt,, drop = FALSE]
  if (nrow(vmp) == 0) {
    stop("trim is set too high, no variables meet that cutoff")
  }
  vmp
}

