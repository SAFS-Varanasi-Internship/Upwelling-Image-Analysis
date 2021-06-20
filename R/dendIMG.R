#' Draw dendrogram with images
#'
#' This is a customized dendrogram plot where images are added to the leaf labels. The
#' function uses functions from the **dendextend** and **stats** libraries.
#'
#' @param centers The centers. Each row is a center and each column is a variable.
#' @param img.list A list of images for each center. Image 1 goes with center row 1, etc.
#' @param type The type of distance to use. This will determine how to calculate the distance between the centers. See details.
#' @param scale.same
#' @param pal The color ramp for the images. This will determine what the images look like.
#' @param lab.extra Extra information to add to the labels. `"mean"` will add the mean value of the center (minus mean over all centers) and `"var"` will add the variance.
#' @param color.labels If TRUE, the labels are blue if below a threshold, black within, and red over.
#' @param color.breaks The breaks to use for the color labels.
#'
#' @details The dendrogram needs the distance between centers and there are many ways to measure
#' this. The default is `type="mean"` to use the euclidian distance
#' `stats::dist(centers)`.  `"mean0"` is
#' similar but the mean of each center is removed (not the mean across centers,
#' the mean of individual centers). If `"correlation"`, then a correlation matrix is used as a
#' distance matrix.
#'
#' Once the distance matrix is calculated, `stats::hclust()` is used to create the cluster and `stats::as.dendrogram()` is used to create the dendrogram object from that. The images are added below the
#' dendrogram plot using `rasterImage()`.
#' 
#' A dendrogram object is returned.
#'
#' @example
#' centers <- matrix(runif(10*4), 10, 4)
#' img.list <- list()
#' for(i in 1:10) img.list[[i]] <- raster::as.raster(matrix(centers[i,],2,2))
#' dendIMG(centers, img.list, lab.extra="none")
#'
#' @export
dendIMG <- function(centers, img.list, type = c("mean0", "original", "correlation"),
                    scale.same = FALSE, pal = colorRamps::matlab.like(100),
                    lab.extra = c("none", "mean", "var"),
                    color.labels = FALSE,
                    color.breaks = c(-2, -0.4, 0.4, 2)) {
  type <- match.arg(type)
  lab.extra <- match.arg(lab.extra)
  # library(dendextend)
  n.K <- nrow(centers)
  lab.extra <- match.arg(lab.extra)
  if (lab.extra == "mean") labs <- paste0("C", 1:n.K, " (", round(apply(centers, 1, mean, na.rm = TRUE) - mean(centers, na.rm = TRUE), digits = 1), ")")
  if (lab.extra == "var") labs <- paste0("C", 1:n.K, " (", round(sqrt(apply(centers, 1, var, na.rm = TRUE)), digits = 2), ")")
  if (lab.extra == "none") labs <- paste0("C", 1:n.K)
  if (type == "original") {
    mat <- centers
    rownames(mat) <- labs
    dd <- stats::dist(mat)
  }
  if (type == "mean0") {
    mat <- t(scale(t(centers), scale = FALSE))
    rownames(mat) <- labs
    dd <- stats::dist(mat)
  }
  if (type == "correlation") {
    mat <- (1 - stats::cor(t(centers))) / 2
    rownames(mat) <- labs
    dd <- stats::as.dist(mat)
  }
  dend <- stats::as.dendrogram(stats::hclust(dd / max(dd)))
  if (color.labels) {
    cols <- cut(apply(centers, 1, mean) - mean(centers), breaks = color.breaks, labels = c("blue", "black", "red"))
    dendextend::labels_colors(dend) <- as.character(cols)[stats::order.dendrogram(dend)]
  }
  # https://rstudio.com/wp-content/uploads/2016/10/how-big-is-your-graph.pdf
  par(mar = c(10, 3, 1, 0), oma = rep(0, 4))
  lab.width <- max(graphics::strwidth(paste0(labs, "a"), units = "inches"))
  # y location of extent of labels
  stats::plot(dend)
  user.range <- par("usr")[c(2, 4)] - par("usr")[c(1, 3)]
  in.to.usr <- par("pin") / user.range # in/usr ratio
  # images will be min inches ((0.8*par("pin")[1]/12.96), 0.8*in.to.usr[1])
  # don't go bigger than what image would be if x range were 12.96 (K=12)
  img.width.in <- min(0.8 * par("pin")[1] / 12.96, 0.8 * in.to.usr[1])
  img.height.in <- img.width.in * nrow(img.list[[1]]) / ncol(img.list[[1]])
  img.width.usr <- img.width.in / in.to.usr[1]
  img.height.usr <- img.height.in / in.to.usr[2]
  # top of the image in usr y measurement
  img.top.usr <- par("usr")[3] - lab.width / in.to.usr[2]
  img.bottom.usr <- img.top.usr - img.height.usr

  for (i in 1:n.K) {
    if (scale.same == FALSE) img <- raster::as.raster(img.list[[stats::order.dendrogram(dend)[i]]], col = pal)
    if (scale.same == TRUE) {
      d <- img.list[[stats::order.dendrogram(dend)[i]]]
      a <- as.numeric(cut(raster::values(d), breaks = seq(min(centers), (max(centers)), (max(centers) - min(centers)) / length(pal))))
      img <- raster::as.raster(img.list[[stats::order.dendrogram(dend)[i]]], col = pal[min(a, na.rm = TRUE):max(a, na.rm = TRUE)])
    }
    img[is.na(img)] <- "#808080"
    # rasterImage(img, i-0.4, -.7, i+0.4, -.5, xpd=TRUE, lty=1, interpolate = FALSE)
    graphics::rasterImage(img, i - img.width.usr / 2, img.top.usr, i + img.width.usr / 2, img.bottom.usr, xpd = TRUE, lty = 1, interpolate = FALSE)
  }
  invisible(dend)
}
