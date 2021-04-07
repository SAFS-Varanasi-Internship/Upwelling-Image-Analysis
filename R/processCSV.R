#' Read in image csv
#' 
#' @description
#' processCSV will read in a csv file of SST and return the matrices needed for `kmeans()`.
#' 
#' @details
#' `stats::kmeans()` wants a matrix where each row is a sample of your data. \So we want each
#' row of our data matrix to be a date and the columns will be the pixels in the image.
#' 
#' @param X
#' @param aspect_ratio
#' @param lat_range What range to subset
#' @param long_range What range to subset
#' @param has.alt If TRUE, then remove 2nd column
#' 
#' @return The function returns `dat.wide` which is the original data (in the specified lat/lon box) 
#' where each row is a date and each column is a pixel in the image grid.
#' `dat.wide` may have NAs (say from land if working with ocean
#' data). `dat.clean` is the data with NA pixels (i.e. land) removed. 
#' `pos.loc` is the location of the
#' non-NA pixels (columns in `dat.wide`) so that the image can be reconstructed after 
#' k clustering is performed.
#' 
#' @export
processCSV <- function(file, aspect_ratio, lat_range, long_range, has.alt=FALSE){
  library(dplyr)

#constants
pixels <- prod(aspect_ratio)

#reads the file
dat <- read.table(file, sep=",", skip=2)
if(has.alt) dat <- dat[,-2]
colnames(dat) <- c("date", "lat", "lon", "sst")
dat$date <- as.Date(dat$date)

# limit to certain box
not.box <- dat$lat < lat_range[1] | dat$lat>lat_range[2] | dat$lon<long_range[1] | dat$lon>long_range[2]
dat.box <- dat[!not.box, ]
n.by.date <- tapply(dat.box$sst, dat.box$date, function(x){sum(is.na(x))})
if(any((n.by.date-n.by.date[1])!=0)) stop("There's a problem. Should be same n for each date.")

dat.wide <- pivot_wider(dat.box, names_from=date, values_from=sst)
pos.loc <- which(!is.na(dat.wide[,3])) # which row are NA?
dat.clean <- na.omit(dat.wide) # remove the rows that are NA

# Note transpose since kmeans() wants variates in columns
return(list(dat=t(dat.wide), dat.clean=t(dat.clean), pos.loc=pos.loc))
}