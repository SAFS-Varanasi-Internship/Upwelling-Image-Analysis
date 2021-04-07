#' Download data from an ERDDAP server
#' 
#' @description
#' Download data using eserver name
#' 
#' @details
#' Go to the server to see what the pars names are and the format. The names for
#' things are unique and can't be predicted.
#'  e.g. http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_26fe_f2c9_ed3b.html
#' 
#' @param id The id of the data. Like hawaii_soest_26fe_f2c9_ed3b
#' @param pars Name of the parameter. Like sst, e.g.
#' @param lat Min-max latitude
#' @param lon Min-max longitude
#' @param date To specify a date range, pass this in like c("2020-01-01", "2020-02-01"). Leave off to download the whole data range.
#' @param altitude Optional. Some data types need this
#' @return The function saves a csv file
#' 
#' @export
getdata <- function(id, pars=NULL, lat=c(7,13), lon=c(72,78), date=NULL, 
                    altitude=10, alt.name="altitude",
                    eserver="https://coastwatch.pfeg.noaa.gov/erddap",
                    datadir = "data"){
  url <- paste0(eserver, "/info/", id, "/index.csv")
  meta <- read.csv(url)
  
  if(!missing(date) && length(date)==1) date <- c(date, date)
  if(missing(date)) date <- c(meta$Value[meta$Attribute.Name=="time_coverage_start"], meta$Value[meta$Attribute.Name=="time_coverage_end"])
  
  lat1 <- lat[1]; lat2 <- lat[2]
  lon1 <- lon[1]; lon2 <- lon[2]
  dfil <- paste0(id, "-", lat1, "-", lat2, "-", lon1, "-", lon2, "-", stringr::str_sub(date[1], 1, 10), "-", stringr::str_sub(date[2], 1, 10), ".csv")
  dfil <- file.path(here::here(), datadir, dfil)
  
  if(!file.exists(dfil)){
    if(missing(pars)){
      pars <- unique(meta$Variable.Name)
      pars <- pars[!(pars %in% c("NC_GLOBAL", "time", "latitude", "longitude", alt.name))]
    }
    # if altitude is req in url, add it
    alttag <- ifelse(alt.name %in% meta$Variable.Name, paste0("[(",altitude,"):1:(",altitude,")]"), "")
    val <- paste0("[(", date[1], "):1:(", date[2], ")]",alttag,"[(",lat1,"):1:(",lat2,")][(",lon1,"):1:(",lon2,")]")
    val2 <- paste0(pars, val, collapse=",")
    url <- paste0(eserver, "/griddap/", id, ".csv?", val2)
    download.file(url, destfile=dfil)
    cat("data saved to", dfil, "\n")
  }else{
    cat("data read from", dfil, "\n")
  }
  dat <- read.csv(file=dfil, stringsAsFactors = FALSE)
  dat <- dat[-1,]
  for(i in 2:ncol(dat)) dat[[i]] <- as.numeric(dat[[i]])
  dat$time <- as.POSIXlt(dat$time, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
  attr(dat, "resolution") <- as.numeric(meta$Value[meta$Attribute.Name=="geospatial_lat_resolution"])
  cat(paste0("data ", id, " date ", date[1], "-", date[2], ", latitude ", lat1, "-", lat2, ", longitude ", lon1, "-", lon2), "\n")
  dat
}