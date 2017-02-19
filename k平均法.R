source("https://raw.githubusercontent.com/futurebridge/RBooks/master/beer.R")
beer


set.seed(100)
beer.mx = as.matrix(beer)
result = kmeans(beer.mx, 3)
result

#それぞれのクラスターの残差平方和
result$withinss
#それぞれのクラスターの残差平方和の合計
result$tot.withinss
#全体データでの残差平方和
result$totss
#全体SS - 個別SSの和
result$betweenss


#maptoolsのインストール
install.packages("maptools")
library(maptools)

plot(beer.mx[,1],beer.mx[,2], col = result$cluster)


par(mfrow=c(1,3));
#クラスターごとに色を変えてプロット
plot(beer.mx[,1],beer.mx[,2], xlab="コク",ylab="かcol = result$cluster)
#重ならないようにラベルを表示
pointLabel(beer.mx,labels=rownames(beer.mx))
#中心点を表示
points(result$centers, col = 1:5, pch = 8)

