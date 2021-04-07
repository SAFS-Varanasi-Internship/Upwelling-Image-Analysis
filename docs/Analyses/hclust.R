yearTable <- function(X2, method=c("kmeans", "hclust.complete", "hclust.ward", "dbscan"), 
                      K=3, iter.max=25, nstart=100, eps=7,
                      dist.type=c("euclidian", "correlation", "euclidian2", "canberra")){
  method=match.arg(method)
  df <- as.data.frame(X2)
  if(dist.type=="correlation"){
    mat <- (1 - cor(t(X2)))/2
    d <- as.dist(mat)
  }
  if(dist.type=="euclidian") d <- dist(df, method = "euclidian")
  if(dist.type=="canberra") d <- dist(df, method = "canberra")
  if(dist.type=="euclidian2") d <- dist(df, method = "euclidian")^2
  if(method=="hclust.complete"){
    clus <- hclust(d, method = "complete" )
    sub_grp <- cutree(clus, k = K)
    centers <- apply (X2, 2, function (x) tapply (x, sub_grp, mean))
  }
  if(method=="hclust.ward"){
    clus <- hclust(d, method = "ward.D2" )
    sub_grp <- cutree(clus, k = K)
    centers <- apply (X2, 2, function (x) tapply (x, sub_grp, mean))
  }
  if(method=="kmeans"){
    out <- kmeans(X2, K, iter.max=iter.max, nstart=nstart)
    sub_grp <- out$cluster
    centers <- out$centers
  }
  if(method=="dbscan"){
    out <- fpc::dbscan(d, eps, MinPts = 5, scale = FALSE, 
           method = "dist")
    sub_grp <- out$cluster
    centers <- apply (X2, 2, function (x) tapply (x, sub_grp, mean))
    K <- length(unique(sub_grp))
  }
  df2 <- data.frame(date=as.Date(rownames(df)), cluster=sub_grp)
  df2$month <- months(df2$date)
  df2$year <- format(df2$date, "%Y")
  n.year <- length(unique(df2$year))
  years <- unique(df2$year)
  dmat <- matrix(NA, n.year, K)
  df2$cluster <- as.factor(df2$cluster)
  for(i in 1:n.year){
    dmat[i,] <- table(df2$cluster[df2$year==years[i]])
  }
  df.year <- as.data.frame(dmat)
  colnames(df.year) <- paste("C",unique(sub_grp))
  df.year$year <- as.numeric(years)
  df.year.long <- tidyr::pivot_longer(df.year, starts_with("C"), names_to="cluster", values_to="count")
  df.year.long <- subset(df.year.long, year<2020)
  library(ggplot2)
  p1 <- ggplot(df.year.long, aes(x=year, y=count)) + geom_point() + 
    facet_wrap(~cluster, scales="free")
  
  # Make images
  return(list(p=p1, centers=centers, clusters=sub_grp))
}

ncomp <- 20
p1 <- yearTable(alpha[mons%in%c("November", "December", "January"), 1:ncomp], 
                method="hclust.complete", K=9, 
                dist.type="euclidian")
p <- p1$p+geom_smooth(method = "lm", se = TRUE)
centers <- p1$centers %*% eigenimages[1:ncomp,, drop=FALSE]
img.list <- imgVectortoRaster(centers, datalist)$list
addIMGtopanel(p, img.list)

# Raw normed data
monvals <- c("November", "December", "January")
for(meth in c("hclust.complete", "hclust.ward", "kmeans")){
p1 <- yearTable(X_norm[mons%in%monvals,], 
                method=meth, K=6, 
                dist.type="euclidian")
p <- p1$p+geom_smooth(method = "lm", se = TRUE)+ggtitle(meth)
centers <- p1$centers
img.list <- imgVectortoRaster(centers, datalist)$list
addIMGtopanel(p, img.list)
}

monvals <- c("March", "April", "May")
for(meth in c("hclust.complete", "hclust.ward", "kmeans")){
  p1 <- yearTable(X_norm[mons%in%monvals,], 
                  method=meth, K=6, 
                  dist.type="euclidian")
  p <- p1$p+geom_smooth(method = "lm", se = TRUE)+ggtitle(meth)
  centers <- p1$centers
  img.list <- imgVectortoRaster(centers, datalist)$list
  addIMGtopanel(p, img.list)
}

monvals <- c("June")
for(meth in c("hclust.complete", "hclust.ward", "kmeans")){
  p1 <- yearTable(X_norm[mons%in%monvals,], 
                  method=meth, K=6, 
                  dist.type="euclidian")
  p <- p1$p+geom_smooth(method = "lm", se = TRUE)+ggtitle(meth)
  centers <- p1$centers
  img.list <- imgVectortoRaster(centers, datalist)$list
  addIMGtopanel(p, img.list)
}

monvals <- c("September", "October")
for(meth in c("hclust.complete", "hclust.ward", "kmeans")){
  p1 <- yearTable(X_norm[mons%in%monvals,], 
                  method=meth, K=6, 
                  dist.type="euclidian")
  p <- p1$p+geom_smooth(method = "lm", se = TRUE)+ggtitle(meth)
  centers <- p1$centers
  img.list <- imgVectortoRaster(centers, datalist)$list
  addIMGtopanel(p, img.list)
}

monvals <- c("July", "August", "September")
for(meth in c("hclust.complete", "hclust.ward", "kmeans")){
  p1 <- yearTable(X_norm[mons%in%monvals,], 
                  method=meth, K=4, 
                  dist.type="euclidian")
  #p <- p1$p+geom_smooth(method = "lm", se = TRUE)+ggtitle(meth)
  p <- p1$p + geom_smooth(span = 0.3)+ggtitle(meth)+ylim(-10,90)
  centers <- p1$centers
  img.list <- imgVectortoRaster(centers, datalist)$list
  addIMGtopanel(p, img.list)
}

# Alpha
ncomp <- 20
monvals <- c("July", "August", "September")
for(meth in c("hclust.complete", "hclust.ward", "kmeans")){
  p1 <- yearTable(alpha[mons%in%monvals,1:ncomp], 
                  method=meth, K=2, 
                  dist.type="euclidian")
  #p <- p1$p+geom_smooth(method = "lm", se = TRUE)+ggtitle(meth)
  p <- p1$p + geom_smooth(span = 0.3)+ggtitle(meth)
  centers <- p1$centers %*% eigenimages[1:ncomp,, drop=FALSE]
  img.list <- imgVectortoRaster(centers, datalist)$list
  addIMGtopanel(p, img.list)
}

# Alpha
ncomp <- 2
monvals <- c("July", "August", "September")
meth <- "dbscan"
  p1 <- yearTable(alpha[mons%in%monvals,1:ncomp], 
                  method=meth, K=9, eps=7,
                  dist.type="euclidian")
  p <- p1$p+geom_smooth(method = "lm", se = TRUE)+ggtitle(meth)
  centers <- p1$centers %*% eigenimages[1:ncomp,, drop=FALSE]
  img.list <- imgVectortoRaster(centers, datalist)$list
  addIMGtopanel(p, img.list)
  
  monvals <- c("July", "August", "September")
  meth <- "dbscan"
  p1 <- yearTable(X_norm[mons%in%monvals,], 
                  method=meth, K=9, eps=9,
                  dist.type="euclidian")
  p <- p1$p+geom_smooth(method = "lm", se = TRUE)+ggtitle(meth)
  centers <- p1$centers
  img.list <- imgVectortoRaster(centers, datalist)$list
  addIMGtopanel(p, img.list)
  


