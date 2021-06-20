#' Add images to a ggplot facet plot
#'
#' This adds images in a image list to the panels in a ggplot object.
#'
#' @param p1 A ggplot object with panels, i.e..
#' @param img.list A list of images for each center. Image 1 goes with panel 1, etc. Panels are numbered across rows. Image are recycled if the list is shorter than the number of panels.
#' @param pal The color ramp for the images. This will determine what the images look like.
#'
#' @details 
#'
#' @example
#' img.list <- list()
#' for(i in 1:3) img.list[[i]] <- raster::as.raster(matrix(runif(4),2,2))
#' 
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x=mpg, y=hp)) + geom_line() + facet_wrap(~cyl)
#' addIMGtopanel(p, img.list)
#'
#' #In this example, the images are cut off at the top.
#' p <- ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
#' p <- mg + facet_grid(vs + am ~ gear, margins = TRUE)
#' addIMGtopanel(p, img.list)

#' @export
addIMGtopanel <- function(p1, img.list, pal = colorRamps::matlab.like(100)){
  library(ggplot2)
  
  plot(p1)
  
  # Get the current viewport tree
  a <- grid::current.vpTree()
  # Get the names of the children viewports with name panel
  # For me, the viewport name of my plot was "layout"; might be different
  #   in different situations
  b <- names(a$children$layout$children)
  # find the names of the panel viewports.
  # Change if you want the images somewhere else in the plot (like panel titles)
  panel.vp <- b[stringr::str_detect(b, "panel-")]
  rows <- unlist(lapply(stringr::str_split(panel.vp, "[.]"), function(x){stringr::str_split(x[2], "-")[[1]][1]}))
  rows <- as.numeric(rows)
  rows <- match(rows, sort(unique(rows)))
  cols <- unlist(lapply(stringr::str_split(panel.vp, "[.]"), function(x){stringr::str_split(x[2], "-")[[1]][2]}))
  cols <- as.numeric(cols)
  cols <- match(cols, sort(unique(cols)))
  
  # set up a viewport for my image; always top left
  vp.img <- grid::viewport(x=unit(0.1,"npc"), y=unit(0.9,"npc"), width=unit(0.2, "npc"), just = "left")
  vp.img2 <- grid::viewport(x=unit(0.3,"npc"), y=unit(0.9,"npc"), width=unit(0.2, "npc"), just = "left")
  vp.img3 <- grid::viewport(x=unit(0.4,"npc"), y=unit(0.9,"npc"), width=unit(0.2, "npc"), just = "left")
  # add the images to each facet
  img.i <- 1
  for(rr in 1:max(rows)){
    for(cc in 1:max(cols)){
      p.vp <- panel.vp[which(rows==rr & cols==cc)]
      # checkout viewport for panel i
      grid::seekViewport(p.vp)
      # draw my image
      grid::grid.draw(grid::grobTree(grid::rasterGrob(raster::as.raster(img.list[[img.i]], col=pal), interpolate=FALSE), vp=vp.img))
      grid::grid.text(img.i, vp=vp.img2)
      img.i <- img.i+1
      if(img.i > length(img.list)) img.i <- 1 # recycle
      if(img.i>length(img.list)) break
    }
  }
}
