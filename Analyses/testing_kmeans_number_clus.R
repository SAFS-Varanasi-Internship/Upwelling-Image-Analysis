out_kmeans_norm <- kmeans(X_norm, 12)
test<- as.matrix(dist(rbind(out_kmeans_norm$centers, X_norm)))
err <- (apply(test[13:nrow(test),1:12],1,min))
err <- ts(err, start=c(1979,1), frequency=12)
plot(decompose(err))
fit <- ets(err, model="ANA")
checkresiduals(fit)

fit <- ets(err, model="ANA")
checkresiduals(fit)

plot(fit)

fit <- auto.arima(err)
checkresiduals(fit)

n_K <- 14
out <- kmeans(X_norm, n_K, nstart=100, iter.max=100)
centers <- out$centers
for(i in 10:12){
  summer.mon <- months(as.Date(rownames(X_norm))) %in% month.name[i]
n_K <- 3
out <- kmeans(X_norm[summer.mon,], n_K, nstart=100, iter.max=100)
centers <- rbind(centers, out$centers)
}
test<- as.matrix(dist(rbind(out$centers, X_norm)))
n_K <- nrow(centers)
err <- (apply(test[(n_K+1):nrow(test),1:n_K],1,min))
err <- ts(err-mean(err, na.rm=TRUE), start=c(1979,1), frequency=12)
acf(err)
library(forecast)
fit <- ets(err, model="ANA")
autoplot(fit)
checkresiduals(fit)

  summer.mon <- months(as.Date(rownames(X_norm))) %in% month.name[1:12]
 # summer.mon <- months(as.Date(rownames(X_norm))) %in% month.name[6:9]
  n_K <- 18
  out <- kmeans(X_norm[summer.mon,], n_K, nstart=100, iter.max=200)
  centers <- out$centers
test<- as.matrix(dist(rbind(out$centers, X_norm[summer.mon,])))
n_K <- nrow(centers)
err <- (apply(test[(n_K+1):nrow(test),1:n_K],1,min))
#err <- ts(err-mean(err, na.rm=TRUE), start=c(1979,1), frequency=12)
err <- ts(err, start=c(1979,1), frequency=12)
acf(err)
library(forecast)
fit <- ets(err, model="ANA")
autoplot(fit)

## chunk by decade
nn <- seq(1,nrow(X_norm)-10*12,10*12)
centers <- c()
for(i in nn){
  summer.mon <- months(as.Date(rownames(X_norm))) %in% month.name[i]
  n_K <- 3
  out <- kmeans(X_norm[i+0:119,], n_K, nstart=100, iter.max=100)
  centers <- rbind(centers, out$centers)
}
test<- as.matrix(dist(rbind(out$centers, X_norm)))
n_K <- nrow(centers)
err <- (apply(test[(n_K+1):nrow(test),1:n_K],1,min))
#err <- ts(err-mean(err, na.rm=TRUE), start=c(1979,1), frequency=12)
err <- ts(err, start=c(1979,1), frequency=12)
acf(err)
library(forecast)
fit <- ets(err, model="ANA")
autoplot(fit)
checkresiduals(fit)


plot(err[seq(11,length(err),12)], type="l",ylim=c(0,20))
for(i in 6:9) lines(err[seq(i,length(err),12)], col="red")
for(i in c(1:2,10:12)) lines(err[seq(i,length(err),12)], col="blue")
for(i in c(3:5)) lines(err[seq(i,length(err),12)], col="green")

n_K <- 10
out_kmeans_norm <- kmeans(X_norm, n_K, nstart=100, iter.max=100)
aa=imgVectortoRaster(out_kmeans_norm$centers, datalist)
actual=imgVectortoRaster(X_norm[1:100,], datalist)
img2 <- list()
for(i in 1:100){
  img2[[i]] <-aa$list[[out_kmeans_norm$cluster[i]]]
}
n = seq(11,100,12)[1:8]
img.stack2 <- raster::stack(c(actual$list[n], img2[n]))
plot(img.stack2, nc=4)

a=outliers.ranking(X_norm)
abc=imgVectortoRaster(X_norm[a$rank.outliers[200:216],], datalist)
plot(abc$stack)

k=3
mon.img=list()
for(i in 1:12){
  summer.mon <- months(as.Date(rownames(X_norm))) %in% month.name[i]
n_K <- k
out <- kmeans(X_norm[summer.mon,], n_K, nstart=100, iter.max=100)
aa=imgVectortoRaster(out$centers, datalist)
mon.img<- c(mon.img, aa$list)
}
nn <- (8*k+1):(12*k)
plot(raster::stack(mon.img[nn]), nc=k)
table(out$cluster)


summer.mon <- months(as.Date(rownames(X_norm))) %in% month.name[7]
fviz_nbclust(X_norm[summer.mon,], kmeans, k.max=10, iter.max=100, nstart=100, method="wss")
fviz_nbclust(X_norm[summer.mon,], kmeans, k.max=10, iter.max=100, nstart=100)
n_K <- 12
out_kmeans_norm <- kmeans(X_norm[summer.mon,], n_K, nstart=1000, iter.max=1000)
test<- as.matrix(dist(rbind(out_kmeans_norm$centers, X_norm[summer.mon,])))
err2 <- (apply(test[(n_K+1):nrow(test),1:n_K],1,min))
err <- rep(NA, nrow(X_norm))
err[summer.mon] <- err2
err <- ts(err-mean(err, na.rm=TRUE), start=c(1979,1), frequency=12)
plot(err); abline(h=0, col="red")

plot(err2)
df <- data.frame(err=err2, t=1:length(err2))
fit.lm <- lm(err~t, data=df)
abline(fit.lm)
summary(fit.lm)
par(mfrow=c(1,2))
acf(err2)
qqnorm(err2); qqline(err2)

acf(err, na.action=na.pass)

fit <- ets(err, model="ANA")
plot(fit)

par(mfrow=c(2,2))
mon <- 1:12
X2 <- X_norm
summer.mon <- months(as.Date(rownames(X2))) %in% month.name[mon]
pp <- list()
for(i in 1:4){
  n_K <- c(4, 6, 12, 14)[i]
  out <- kmeans(X2[summer.mon,], n_K, nstart=1000, iter.max=1000)
  mat <- rbind(out$centers, X2[summer.mon,])
  test<- as.matrix(dist(mat))
  err <- (apply(test[(n_K+1):nrow(test),1:n_K],1,min))
  err <- ts(err-mean(err, na.rm=TRUE), start=c(1979,1), frequency=12)
  #qqnorm(sqrt(err2), main=n_K); qqline(sqrt(err2))
  #acf(sqrt(err2))
  # plot(err2)
  # df <- data.frame(err=err2, t=1:length(err2))
  # fit.lm <- lm(err~t, data=df)
  # abline(fit.lm)
  fit <- ets(err, model="ANA")
  pp[[i]] <- ggplot2::autoplot(fit) + ggtitle(n_K)
}
gridExtra::grid.arrange(grobs=pp)

par(mfrow=c(2,2))
mon <- 1:12
X2 <- X_norm
summer.mon <- months(as.Date(rownames(X2))) %in% month.name[mon]
pp <- list()
for(i in 1:4){
  n_K <- c(16, 18, 20, 22)[i]
  out <- kmeans(X2[summer.mon,], n_K, nstart=1000, iter.max=1000)
  test<- as.matrix(dist(rbind(out$centers, X2[summer.mon,])))
  err <- (apply(test[(n_K+1):nrow(test),1:n_K],1,min))
  err <- ts(err-mean(err, na.rm=TRUE), start=c(1979,1), frequency=12)
  #qqnorm(sqrt(err2), main=n_K); qqline(sqrt(err2))
  #acf(sqrt(err2))
  # plot(err2)
  # df <- data.frame(err=err2, t=1:length(err2))
  # fit.lm <- lm(err~t, data=df)
  # abline(fit.lm)
  fit <- ets(err, model="ANA")
  pp[[i]] <- ggplot2::autoplot(fit) + ggtitle(n_K)
}
gridExtra::grid.arrange(grobs=pp)


# Dissimilarity matrix
d <- dist(X_norm, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

hc2 <- agnes(X_norm, method = "complete")

# Agglomerative coefficient
hc2$ac

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(X_norm, method = x)$ac
}

map_dbl(m, ac)

df <- as.data.frame(X_norm)
d <- dist(df, method = "euclidean")
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 6)

# Number of members in each cluster
table(sub_grp)

df2 <- data.frame(date=as.Date(rownames(df)), cluster=sub_grp)
df2$month <- months(df2$date)
ggplot(df2, aes(cluster)) + geom_histogram() + facet_wrap(~month)

#to get centroids
centroids <- apply (df, 2, function (x) tapply (x, sub_grp, mean))

pivot_wider(select(df2, -date), names_from=month, values_from=cluster)

fviz_cluster(list(data = df, cluster = sub_grp))

fviz_nbclust(df, FUN = hcut, method = "wss")

jaccard <- function(df, month1, month2) {
  sums = rowSums(M[,c(user1, user2)])
  
  similarity = length(sums[sums==2])
  total = length(sums[sums==1]) + similarity
  
  similarity/total
}

df <- as.data.frame(X_norm)
d <- dist(df, method = "euclidean")
hc5 <- hclust(d, method = "ward.D2" )
K=6
# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = K)
df2 <- data.frame(date=as.Date(rownames(df)), cluster=sub_grp)
df2$month <- months(df2$date)
df2$year <- format(df2$date, "%Y")


dmat <- matrix(NA, 12, K)
df2$cluster <- as.factor(df2$cluster)
for(i in 1:12){
  dmat[i,] <- table(df2$cluster[df2$month==month.name[i]])
}
dd <- dist(dmat)
hc5 <- hclust(dd, method = "ward.D2" )
plot(hc5)
barplot(hc5$height,
        names.arg = (nrow(X_norm) - 1):1 # show the number of cluster below each bars
)
plot(diff(hc5$height), xlim=c(450,500))

df <- as.data.frame(X_norm)
d <- dist(df, method = "euclidean")
hc5 <- hclust(d, method = "ward.D2" )
K=3
# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = K)
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
colnames(df.year) <- paste("C",1:K)
df.year$year <- as.numeric(years)
df.year.long <- tidyr::pivot_longer(df.year, starts_with("C"), names_to="cluster", values_to="count")
df.year.long <- subset(df.year.long, year<2020)
library(ggplot2)
ggplot(df.year.long, aes(x=year, y=count)) + geom_line() + geom_point() + facet_wrap(~cluster)



rownames(dmat) <- years
dd <- dist(dmat)
hc5 <- hclust(dd, method = "ward.D2" )
plot(hc5)

n.year <- length(unique(df2$year))
dmat <- matrix(NA, n.year, K)
df2$cluster <- as.factor(df2$cluster)
for(i in 1:n.year){
  dmat[i,] <- table(df2$cluster[df2$month==month.name[i]])
}
dd <- dist(dmat)
hc5 <- hclust(dd, method = "ward.D2" )
plot(hc5)

# get different indices
index_out=list()
for(i in c("kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")){
tmp <- try(NbClust(
  data = X_norm,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 15, # maximum number of clusters
  method = "kmeans", # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
  index = i
))
if(inherits(tmp, "try-error")) next
index_out[[i]] <- tmp
}

