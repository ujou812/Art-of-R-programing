#成績の主成分分析試し

#データサンプルurl(http://www.statistics.co.jp/index.html)

result <- prcomp(seiseki, scale=T)
summary(result)
biplot(result)

result2 <- prcomp(seiseki, scale=F)
summary(result2)

result$rotation
result$x

fc.l <- sweep(result$rotation, MARGIN=2, result$sdev, FUN="*")
subject <- c("国", "社", "数", "理", "音", "美", "体", "技", "英")
plot(fc.l[,1], pch=subject, ylim=c(-1,1), main="PC1")
plot(fc.l[,2], pch=subject, ylim=c(-1,1), main="PC2")

