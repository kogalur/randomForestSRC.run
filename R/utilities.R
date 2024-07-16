#' Helper Function
#'
#' This function helps the main function.
#'
#' @export

plot.tune.nodesize <- function(result, multiplier = 1,
            xlab = "Nodesize", ylab = "Error Rate", title = TRUE) {
  g <- ggplot(result$err, aes(x = !!sym("nodesize"), y = !!sym("err"))) +
    geom_point() + geom_smooth(method = "loess", se = TRUE) +  
    labs(x = xlab, y = ylab) + theme_minimal() 
  g <- theme.error(g, multiplier)
  
  ## return the ggplot
  if (title) {
    g + labs(title = "Error Rate Over Nodesize")
  }
  else {
    g
  }
}



get.add.ci <- function(dots) {
  if (is.null(dots$add.ci)) {
    TRUE
  }
  else {
    dots$add.ci
  }
}

get.show.plots <- function(dots) {
  if (is.null(dots$show.plots)) {
    TRUE
  }
  else {
    dots$show.plots
  }
}

get.verbose <- function(dots) {
  if (is.null(dots$verbose)) {
    TRUE
  }
  else {
    dots$verbose
  }
}


mysampsize <- function(x) {min(x * .632, x ^ (4/5))}
