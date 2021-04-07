## Varimax

H_inv <- varimax(sst.pca$x)$rotmat
## rotate factor loadings
Z_rot = sst.pca$x %*% H_inv   
## rotate processes
loadings = solve(H_inv) %*% sst.pca$rotation
ncomp <- 100

# rawLoadings     <- sst.pca$rotation[,1:ncomp] %*% diag(sst.pca$sdev, ncomp, ncomp)
# rotatedLoadings <- varimax(rawLoadings)$loadings
# invLoadings     <- t(pracma::pinv(rotatedLoadings))
# scores          <- scale(X_norm) %*% invLoadings

rawLoadings     <- sst.pca$rotation %*% diag(sst.pca$sdev)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- X_norm %*% invLoadings
colnames(scores) <- paste0("PC", 1:ncol(scores))

img.list <- imgVectortoRaster(t(rotatedLoadings), datalist)$list
p_stack <- stack(img.list[[1]], img.list[[2]], img.list[[3]], 
                 img.list[[4]], img.list[[5]], img.list[[6]])
library(tmap)
pal <- colorRamps::matlab.like(100)
tm_shape(p_stack) + 
  tm_raster(style= "cont", title="SST Anomaly", 
            palette=pal, midpoint=NA, 
            colorNA = "grey", textNA = "Land") +
  tm_layout(panel.labels = paste("PC", 1:length(p_stack))) +
  tm_layout(main.title = "Eigen Images", title.size = 1)

# Set up the data frame
library(tidyr)
df.rot <- data.frame(scores,
                 date=as.Date(rownames(X_norm)),
                 year=as.integer(format(as.Date(rownames(X_norm)), "%Y")),
                 mon=factor(format(as.Date(rownames(X_norm)), "%b"), levels=month.abb),
                 decade=cut(as.integer(format(as.Date(rownames(X_norm)), "%Y")), breaks=seq(1970,2020,10), labels=c("71-80", "81-90", "91-00", "01-10", "11-20")))
df2.rot = pivot_longer(df.rot, starts_with("PC"), names_to="PC", values_to="value")

p <- ggplot(subset(df2.rot, PC %in% c("PC2")), aes(x=date, y=value, fill=value>0)) +
  geom_col(width=300) +
  facet_wrap(~mon) +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  ggtitle("PC3 loadings")
p

rawLoadings     <- sst.pca$rotation %*% diag(sst.pca$sdev)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(X_norm) %*% invLoadings
colnames(scores) <- paste0("PC", 1:ncol(scores))

img.list <- imgVectortoRaster(t(rotatedLoadings), datalist)$list
p_stack <- stack(img.list[[1]], img.list[[2]], img.list[[3]], 
                 img.list[[4]], img.list[[5]], img.list[[6]])
library(tmap)
pal <- colorRamps::matlab.like(100)
tm_shape(p_stack) + 
  tm_raster(style= "cont", title="SST Anomaly", 
            palette=pal, midpoint=NA, 
            colorNA = "grey", textNA = "Land") +
  tm_layout(panel.labels = paste("PC", 1:length(p_stack))) +
  tm_layout(main.title = "Eigen Images", title.size = 1)

library(tidyr)
df <- data.frame(scores,
                 date=as.Date(rownames(X_norm)),
                 year=as.integer(format(as.Date(rownames(X_norm)), "%Y")),
                 mon=factor(format(as.Date(rownames(X_norm)), "%b"), levels=month.abb))
df2 = pivot_longer(df, starts_with("PC"), names_to="PC", values_to="value")
library(ggplot2)
plist <- list()
for(i in month.name){
p <- ggplot(subset(df2, PC %in% paste0("PC",1:2) & mon=="Mar"), aes(x=date, y=value, fill=value>0)) +
  geom_col(width=300) + ylim(-10,10) +
  facet_wrap(~PC)
plist[[i]] <- p
}
library(gridExtra)


p <- ggplot(subset(df2, PC %in% paste0("PC",2)), aes(x=date, y=value, fill=value>0)) +
  geom_col(width=300) +
  facet_wrap(~mon)
p

p <- ggplot(df, aes(x=PC1, y=PC2, col=year)) +
  geom_point() +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  annotate("text", x=-5, y=10, label="cold tip") +
  annotate("text", x=11, y=0, label="upwelling", angle=90) +
  scale_colour_gradient(low = "red", high = "blue", na.value = NA) +
  facet_wrap(~mon)
p

p <- ggplot(df, aes(x=PC1, y=PC2, col=year)) +
  geom_point() +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  scale_colour_gradientn(colors=terrain.colors(40, rev=TRUE)) +
  facet_wrap(~mon)
p

p <- ggplot(subset(df, decade%in%c("81-90","11-20")), aes(x=PC1, y=PC2, col=decade)) +
  geom_point() +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  facet_wrap(~mon) +
  ggtitle("1980s versus 2010s")
p

#############

img.stack <- imgVectortoRaster(t(sst.pca$rotation[,1:2]%*%matrix(c(-5,5),ncol=1)), datalist)$stack
plot(img.stack)
library(tmap)
pal <- colorRamps::matlab.like(100)
tm_shape(p_stack) + 
  tm_raster(style= "cont", title="SST Anomaly", 
            palette=pal, midpoint=NA, 
            colorNA = "grey", textNA = "Land") +
  tm_layout(panel.labels = paste("PC", 1:length(p_stack))) +
  tm_layout(main.title = "Eigen Images", title.size = 1)

p <- ggplot(df, aes(x=PC1, y=PC2)) + geom_point(col=NA) + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0)
for(x in list(c(-5,-5), c(-5,5), c(-2.5, -2.5), 
              c(-2.5,0), c(-2.5,3), c(10,5), 
              c(8,0), c(2.5,0), c(5,4), c(5,-7.5),
              c(8,-5), c(-5,-8), c(5,-12), c(5,-2))){
    img.list <- imgVectortoRaster(t(sst.pca$rotation[,1:2] %*% matrix(x, ncol=1)), datalist)$list
    img <- raster::as.raster(img.list[[1]])
    img[is.na(img)] <- "#808080"
    g <- grid::rasterGrob(img, interpolate=TRUE)
    p <- p +
      annotation_custom(g, xmin=x[1]-2, xmax=x[1]+2, ymin=x[2]-2, ymax=x[2]+2)
}
p2 <- ggplot(df, aes(x=PC1, y=PC2, col=mon)) + 
  geom_point() + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  ylab(expression(paste("PC2 cold tip (", alpha[2], ")"))) +
  xlab(expression(paste("PC1 upwelling (", alpha[1], ")"))) +
  ggtitle("weights on PC1 and PC2")
grid.arrange(p2, 
             p+ggtitle(expression(paste(`image = `, alpha[1], lambda[1], `+`, alpha[2], lambda[2]))), 
             nrow=1, widths=c(.58,.42))

ncomp <- 1
RE1 <- t(sst.pca$rotation[,1:ncomp] %*% t(sst.pca$x[,1:ncomp]))
ncomp <- 2
RE2 <- t(sst.pca$rotation[,1:ncomp] %*% t(sst.pca$x[,1:ncomp]))
ncomp <- 20
RE3 <- t(sst.pca$rotation[,1:ncomp] %*% t(sst.pca$x[,1:ncomp]))
i <- 20
img1 <- imgVectortoRaster(rbind(X_norm[i,], RE1[i,], RE2[i,], RE3[i,]), datalist)$stack
tm_shape(img1) + 
  tm_raster(style= "cont", title="SST Anomaly", 
            palette=pal, midpoint=NA, 
            colorNA = "grey", textNA = "Land") +
  tm_layout(panel.labels = c("True", "1 PC", "2 PC", "3 PC"),
            title=rownames(X_norm)[i])

rawLoadings     <- sst.pca$rotation
rotatedLoadings <- varimax(rawLoadings, normalize=FALSE)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- X_norm %*% invLoadings

H_inv <- varimax(sst.pca$x)$rotmat
## rotate factor loadings
scores = sst.pca$x %*% H_inv
## rotate processes
rotatedLoadings = t(solve(H_inv)) %*% sst.pca$rotation

rawLoadings     <- sst.pca$rotation %*% diag(sst.pca$sdev)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
#scores          <- scale(X_norm) %*% invLoadings
scores          <- X_norm %*% invLoadings

ncomp <- 1
RE1 <- t(rotatedLoadings[,1:ncomp] %*% t(scores[,1:ncomp]))
ncomp <- 2
RE2 <- t(rotatedLoadings[,1:ncomp] %*% t(scores[,1:ncomp]))
ncomp <- 3
RE3 <- t(rotatedLoadings[,1:ncomp] %*% t(scores[,1:ncomp]))
i <- 8
img1 <- imgVectortoRaster(rbind(X_norm[i,], RE1[i,], RE2[i,], RE3[i,]), datalist)$stack
tm_shape(img1) + 
  tm_raster(style= "cont", title="SST Anomaly", 
            palette=pal, midpoint=NA, 
            colorNA = "grey", textNA = "Land") +
  tm_layout(panel.labels = c("True", "1 PC", "2 PC", "3 PC"),
            title=rownames(X_norm)[i])

ncomp <- 10
df4 <- subset(df2, PC %in% paste0("PC", 1:ncomp))
df4$PC <- factor(df4$PC, level=paste0("PC", 1:ncomp))
df4 <- df4 %>% group_by(PC, mon) %>%
  mutate(mean=mean(value),
         err=value-mean)
a <- df4 %>% group_by(mon, PC) %>%
  summarize(mean=mean(mean)) %>%
  subset(mon=="Sep")
df4$augerr <- df4$value-rep(a$mean,498)
df5 <- df4 %>% group_by(mon, date) %>% 
  summarize(distance=sqrt(sum(err^2)),
            augdist=sqrt(sum(augerr^2)))
df5 %>% group_by(mon, PC) %>%
  summarize(dist=mean(distance)) %>%
  plot

  

ggplot(df5, aes(x=date, y=distance)) + geom_point() +
  geom_hline(yintercept=3, col="red") +
  geom_smooth(span = 0.9) + xlab("") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ggtitle("Distance from the average image") +
  facet_wrap(~mon)

loc <- which(rownames(X_norm)%in%paste0(seq(1980,2019,2),"-05-01"))
img1 <- imgVectortoRaster(X_norm[loc,], datalist)$stack
tm_shape(img1) + 
  tm_raster(style= "cont", title="SST Anomaly", 
            palette=pal, midpoint=NA, 
            colorNA = "grey", textNA = "Land") +
  tm_layout(panel.labels = rownames(X_norm)[loc])

loc <- which(rownames(X_norm)%in%paste0(seq(1980,2019,2),"-05-01"))
img1 <- imgVectortoRaster(RE3[loc,], datalist)$stack
tm_shape(img1) + 
  tm_raster(style= "cont", title="SST Anomaly", 
            palette=pal, midpoint=NA, 
            colorNA = "grey", textNA = "Land") +
  tm_layout(panel.labels = rownames(X_norm)[loc],
            title=format(as.Date(rownames(X_norm)[loc[1]]), "%B"))

ggplot(df5, aes(x=date, y=augdist)) + geom_point() +
  geom_hline(yintercept=3, col="red") +
  geom_smooth(span = 0.9) + xlab("") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ggtitle("Distance from the average image") +
  facet_wrap(~mon)

loc <- which(rownames(X_norm)%in%paste0(1980:2020,"-05-01"))
img1 <- imgVectortoRaster(matrix(apply(X_norm[loc,],2,mean),nrow=1), datalist)$stack
tm_shape(img1) + 
  tm_raster(style= "cont", title="SST Anomaly", 
            palette=pal, midpoint=NA, 
            colorNA = "grey", textNA = "Land") +
  tm_layout(panel.labels = rownames(X_norm)[loc])

loc <- which(rownames(X_norm)%in%paste0(1980:2020,"-03-01"))
dist.mean <- dist(rbind(matrix(apply(X_norm[loc,],2,mean),nrow=1), X_norm[loc,]))
plot(as.matrix(dist.mean)[-1,1], ylim=c(0,10))
