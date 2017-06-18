
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

#ギャップ統計量を求める
result = clusGap(beer, kmeans, K.max = 10, B = 100, verbose = interactive())
result
plot(result)

#ワインデータの読み込み
wine = read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data')

colnames(wine)=c('クラス','アルコール','リンゴ酸','灰','灰のアルカリ度','マグネシウム','フェノール','フラボノイド','ノンフラボノイド',
                 'ポリフェノール','色','色相','OD280/OD315','プロリン')

#クラスを外してクラスタリング
wine2 = wine[setdiff(colnames(wine), "クラス")]

#k平均法でクラスタリング
result = kmeans(wine2,5,iter.max = 500)

#usefulパッケージのインストール
install.packages("useful")
library(useful)
plot(result,data=wine2)


result2 = clusGap(wine2, kmeans, K.max = 10, B = 100, verbose = interactive())
result2


#clusterパッケージのインストール
install.packages("cluster")
library(cluster)

#距離を求める
w.dist = dist(wine2)^2
#シルエット係数を求める
sil = silhouette (result$cluster, w.dist)
plot(sil)

#mclust
#mclustパッケージのインストール
install.packages("mclust")
library(mclust)

wine.mclust = Mclust(wine2)
summary(wine.mclust)
wine.mclust$BIC
plot(wine.mclust)

