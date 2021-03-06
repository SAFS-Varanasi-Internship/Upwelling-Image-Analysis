dendIMG <- function(centers, img.list, type=c("mean0", "original", "correlation"), 
                    scale.same=FALSE, pal=colorRamps::matlab.like(20)){
  library(dendextend)
  par(mar=c(10,3,1,0))
  labs <- paste0("C", 1:n_K, "  ", round(apply(centers,1,mean)-mean(centers),digits=1))
  if(type=="original"){
    mat <- centers
    rownames(mat) <- labs
    dd <- dist(mat)
  }
  if(type=="mean0"){
    mat <- t(scale(t(centers), scale=FALSE))
    rownames(mat) <- labs
    dd <- dist(mat)
  }
  if(type=="correlation"){
    mat <- (1 - cor(t(CC)))/2
    rownames(mat) <- labs
    dd <- as.dist(mat)
  }
  dend <- as.dendrogram(hclust(dd/max(dd)))
  cols <- cut(apply(CC,1,mean)-mean(CC), breaks=c(-2,-0.4,0.4,2), labels=c("blue", "black", "red"))
  labels_colors(dend) <- as.character(cols)[order.dendrogram(dend)]
  plot(dend)
  for(i in 1:12){
    if(scale.same==FALSE) img <- as.raster(img.list[[order.dendrogram(dend)[i]]], col=pal)
    if(scale.same==TRUE){
      d <- img.list[[order.dendrogram(dend)[i]]]
      a <- as.numeric(cut(values(d), breaks=seq(min(centers),(max(centers)),(max(centers)-min(centers))/length(pal))))
      img <- as.raster(img.list[[order.dendrogram(dend)[i]]], col=pal[min(a, na.rm=TRUE):max(a, na.rm=TRUE)])
    }
    img[is.na(img)] <- "#808080"
    rasterImage(img, i-0.4, -.7, i+0.4, -.5, xpd=TRUE, lty=1, interpolate = FALSE)
  }
}
