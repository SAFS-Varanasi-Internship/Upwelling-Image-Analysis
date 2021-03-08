myheatmap <- function(dend, kobject, main.n=3, sub.n=7, ramp=c("mean", "var"), 
                      ramp.type=1){
  cluster <- kobject$cluster
  CC <- kobject$centers
  library(ggplot2)
  df <- data.frame(date=as.Date(names(cluster)), cluster=as.factor(cluster))
  df$month <- factor(months(df$date), levels=month.name[1:12])
  df$year <- as.numeric(format(df$date,'%Y'))
  
dend.group <- cutree(dend, k=sub.n)
for(i in 1:length(dend.group)) img.group[img.group==i] <- dend.group[i]
df$sub.group <- as.factor(img.group)

# Set up my palette; ramp within the main group
dend.group.main <- cutree(dend, k=main.n)
pal.sub <- rep(NA, attr(dend, "members"))
pals <- c("YlOrRd", "Greys", "Greens", "BuPu", "Blues")
pals[main.n] <- "Blues"
temps <- apply(CC,1,get(ramp))
main.temps <- tapply(temps, dend.group.main, mean)
tmp.main <- sort(main.temps, index.return=TRUE, decreasing=TRUE)$ix
if(ramp.type==1){
for(i in 1:main.n){
  main.group <- dend.group.main==tmp.main[i]
  sub.group <- unique(dend.group[main.group])
  cols <- rev(RColorBrewer::brewer.pal(max(3, length(sub.group)+2), pals[i]))
  sub.temps <- tapply(temps[main.group], dend.group[main.group], mean)
  tmp <- sort(sub.temps, index.return=TRUE, decreasing=TRUE)$ix
  for(j in 1:length(sub.group))
    pal.sub[dend.group==sub.group[tmp[j]]] <- cols[j]
}
}else{
  pal.sub <- rep(NA, main.n)
  main.temps <- tapply(apply(CC,1,mean), dend.group.main, mean)
  tmp.main <- sort(main.temps, index.return=TRUE, decreasing=TRUE)$ix
for(i in 1:main.n){
  pal.sub[tmp.main[i]] <- rev(RColorBrewer::brewer.pal(3, pals[i]))[1]
}
pal.sub <- pal.sub[dend.group.main]
temp.scaled <- 0.2+(temps-min(temps))/((max(temps)-min(temps))*1.25)
for(i in 1:length(pal.sub)) pal.sub[i] <- desat(pal.sub[i], sat=temp.scaled[i])
}

# Plot
ggplot(df, aes(x=year, y=month, fill= cluster)) + geom_tile() +
  scale_fill_manual(values=pal.sub)
}
