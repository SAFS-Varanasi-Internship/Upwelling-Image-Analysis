## Function for desaturating colors by specified proportion
## https://stackoverflow.com/questions/26314701/r-reducing-colour-saturation-of-a-colour-palette
desat <- function(cols, sat=0.5) {
  #library(colorspace)   ## hsv colorspace manipulations
  #library(RColorBrewer) ## For some example colors
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}