#load(datafile)
#datalist <- out
imgVectortoRaster <- function(centers, datalist){
  n_K <- nrow(centers)
  Data_dirty <- datalist$dat
  lats <- Data_dirty[1,]
  lons <- Data_dirty[2,]
  asp <- c(table(lons)[1], table(lats)[1]) # lon (x) - lats (y)
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
