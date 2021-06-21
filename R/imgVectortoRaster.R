#' Convert vector images to rasters
#'
#' Takes a matrix where each row is an image in vector form and turns that into raster images. This
#' function is specific to the data format for the upwelling project.
#'
#' @param centers a matrix where each row is an image in vector form. It is assumed to be only
#'  the cleaned (no land) images.
#' @param datalist a data list with information about the positive locations (non-land).
#'
#' @details 
#' 
#' `datalist` is a list with `dat`, `dat.clean`, and `pos.loc`. `dat` and `dat.clean` are 
#' vectorized images, meaning a raster image is made into a vector by concatenating each row of 
#' the raster into a rows $\times$ columns vector. The first 2 rows of `dat` and `dat.clean` are 
#' the latitude and longitude values. `dat` has the land (NAs) while `dat.clean` has the land 
#' removed so is smaller. `pos.loc` is the location of the positive values (not land) in `dat`. 
#' `pos.loc` is used to allow you to reconstruct the image from `dat.clean`.
#'
#' @export
imgVectortoRaster <- function(centers, datalist){
  n_K <- nrow(centers)
  Data_dirty <- datalist$dat
  lats <- Data_dirty[1,]
  lons <- Data_dirty[2,]
  asp <- c(length(unique(lons)), length(unique(lats))) # c(width (cols), height (rows))
  bb.box <- c(min(lons), max(lons), min(lats), max(lats))
  centroid_images <- matrix(NA, n_K, ncol(Data_dirty))
  centroid_images[, datalist$pos.loc] <- centers
  rownames(centroid_images) <- paste("Centroid", 1:n_K)
  img.list <- list()
  img.mat <- centroid_images
  for(i in 1:nrow(img.mat)){
    tmp <- matrix(img.mat[i,], asp[2], asp[1], byrow=TRUE)
    tmp <- tmp[asp[2]:1, ] # lat 7 at bottom not top
    tmp <- raster::raster(tmp)
    raster::extent(tmp) <- bb.box
    img.list[[i]] <- tmp
  }
  img.stack <- raster::stack(img.list)
  names(img.stack) <- paste("Centroid", 1:n_K)
  raster::crs(img.stack) <- "+proj=longlat +datum=WGS84"
  return(list(list=img.list, stack=img.stack))
}
