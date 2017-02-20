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


#クラスターごとに色を変えてプロット
plot(beer.mx, col = result$cluster)
#重ならないようにラベルを表示
pointLabel(beer.mx,labels=rownames(beer.mx))
#中心点を表示
points(result$centers, col = 1:5, pch = 8)

#clusterパッケージのインストール
install.packages("cluster")
library(cluster)

result = clusGap(beer, kmeans, K.max = 10, B = 100, verbose = interactive())
result
plot(result)