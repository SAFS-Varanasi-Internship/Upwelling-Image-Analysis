#' Desaturate colors
#'
#' Function for desaturating colors by specified proportion. Reference: 
#' https://stackoverflow.com/questions/26314701/r-reducing-colour-saturation-of-a-colour-palette
#' 
#' See the **colorspace** library for examples of `hsv()` colorspace manipulations and
#' **RColorBrewer** library for some example colors
#' 
#' @param cols a vector of colors
#' @param sat saturation level
#' 
#' @return A list of desaturated colors.
#' 
#' @export
desat <- function(cols, sat=0.5) {
  X <- diag(c(1, sat, 1)) %*% grDevices::rgb2hsv(grDevices::col2rgb(cols))
  grDevices::hsv(X[1,], X[2,], X[3,])
}