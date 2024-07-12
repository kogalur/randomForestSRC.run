#' Helper Function
#'
#' This function helps the main function.
#'
#' @export

theme.cr.legend <- function(g, multiplier1 = 1, multiplier2 = 1, multiplier3 = 1) {
  g + theme(
    axis.title.x = element_text(size=rel(multiplier1 * .8)),
    axis.ticks.x = element_blank(),
    #axis.title.y = element_text(margin = margin(l = 20), size=rel(multiplier * 1)),
    #axis.title.y = element_blank(),
    axis.title.y = element_text(size=rel(multiplier1 * .8)),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = multiplier2 * 6),  
    legend.key.size = unit(0.5, "lines"),               
    legend.spacing.x = unit(0.2, "cm"),                 
    legend.spacing.y = unit(0.2, "cm"),                 
    plot.title = element_text(size = multiplier3 * 10)   
    ) +
    guides(color = guide_legend(ncol = 1))             
}

theme.cr.legend.adjust <- function(g, cr, x=.85, y=.75) {

  # Calculate the range of the data
  x_range <- range(cr$time)
  y_range <- range(cr$value)
  
  # Function to convert data range to relative position
  relative_position <- function(value, range) {
    (value - range[1]) / diff(range)
  }
  
  # Set legend position dynamically
  legend_x <- relative_position(x_range[1] + x * diff(x_range), x_range)
  legend_y <- relative_position(y_range[1] + y * diff(y_range), y_range)
  
  # Adjust the plot with dynamic legend position
  g + theme(legend.position = c(legend_x, legend_y))
  
}

theme.error <- function(g, multiplier = 1) {
  g + theme(
    plot.title = element_text(size=rel(multiplier * 1.2 * multiplier)),
    axis.title.x = element_text(size=rel(multiplier * multiplier)),
    axis.title.y = element_text(size=rel(multiplier * multiplier))
 )
}


theme.error.legend <- function(g, multiplier1 = 1, multiplier2 = 1, multiplier3 = 1) {
  g + theme(
    axis.title.x = element_text(size=rel(multiplier1 * .8)),
    axis.ticks.x = element_blank(),
    #axis.title.y = element_text(margin = margin(l = 20), size=rel(multiplier * 1)),
    #axis.title.y = element_blank(),
    axis.title.y = element_text(size=rel(multiplier1 * .8)),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = multiplier2 * 6),  
    legend.key.size = unit(0.5, "lines"),               
    legend.spacing.x = unit(0.2, "cm"),                 
    legend.spacing.y = unit(0.2, "cm"),                 
    plot.title = element_text(size = multiplier3 * 10)   
    ) +
    guides(color = guide_legend(ncol = 1))             
}

theme.error.legend.adjust <- function(g, err, x=.85, y=.75) {

  # Calculate the range of the data
  x_range <- range(err$trees)
  y_range <- range(err$value)
  
  # Function to convert data range to relative position
  relative_position <- function(value, range) {
    (value - range[1]) / diff(range)
  }
  
  # Set legend position dynamically
  legend_x <- relative_position(x_range[1] + x * diff(x_range), x_range)
  legend_y <- relative_position(y_range[1] + y * diff(y_range), y_range)
  
  # Adjust the plot with dynamic legend position
  g + theme(legend.position = c(legend_x, legend_y))
  
}


theme.vimp <- function(g, multiplier1 = 1, multiplier2 = .6) {
  g + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size=rel(multiplier1)),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size=rel(multiplier2)),
    )        
}

theme.vimp.ci <- function(g, multiplier1 = 1, multiplier2 = .6) {
  g + theme(
    axis.title.x = element_text(size=rel(1.2 * multiplier1)),
    axis.text.x = element_text(size=rel(multiplier1)),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size=rel(multiplier1)),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size=rel(multiplier2)),
    )
}



theme.md <- function(g, multiplier1 = 1, multiplier2 = .6) {
  g + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=rel(multiplier1)),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size=rel(multiplier1)),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size=rel(multiplier2)),
    )        
}
