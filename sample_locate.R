library(ggplot2)
library(grid)

p <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

locator <- function(p) {
  # Build ggplot object
  ggobj <- ggplot_build(p)
  
  # Extract coordinates
  xr <- ggobj$layout$panel_ranges[[1]]$x.range
  yr <- ggobj$layout$panel_ranges[[1]]$y.range
  
  # Variable for selected points
  selection <- data.frame(x = as.numeric(), y = as.numeric())
  colnames(selection) <- c(ggobj$plot$mapping$x, ggobj$plot$mapping$y)
  
  # Detect and move to plot area viewport
  suppressWarnings(print(ggobj$plot))
  panels <- unlist(current.vpTree()) %>%
    grep("panel", ., fixed = TRUE, value = TRUE)
  p_n <- length(panels)
  seekViewport(panels, recording=TRUE)
  pushViewport(viewport(width=1, height=1))
  
  # Select point, plot, store and repeat
  for (i in 1:10){
    tmp <- grid.locator('native')
    if (is.null(tmp)) break
    #grid.points(tmp$x,tmp$y, pch = 16, gp=gpar(cex=0.5, col="darkred"))
    str(tmp)
    selection[i, ] <- as.numeric(tmp)
  }
  #grid.polygon(x= unit(selection[,1], "native"), y= unit(selection[,2], "native"), gp=gpar(fill=NA))
  #return a data frame with the coordinates of the selection
  return(selection)
}

aaa <- locator(p)