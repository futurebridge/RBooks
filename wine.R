wine = read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data')

colnames(wine)=c('クラス','アルコール','リンゴ酸','灰','灰のアルカリ度','マグネシウム','フェノール','フラボノイド','ノンフラボノイド',
                 'ポリフェノール','色','色相','OD280/OD315','プロリン')

#クラスを外してクラスタリング
wine2 = wine[setdiff(colnames(wine), "クラス")]

#k平均法でクラスタリング
result = kmeans(wine2,3,iter.max = 500)

#usefulパッケージのインストール
install.packages("useful")
library(useful)
plot(result,data=wine2)

#k平均法でのクラスタリング結果(result$cluster)と正解データwine["クラス"]をresult.mとして合成
(result.m = rbind(table(data.frame(result$cluster)),table(wine["クラス"])))
#誤検知率を求める=(クラスタリング結果-正解データの絶対値)を全体数で割る
sum(abs(result.m[1,]-result.m[2,]))/sum(table(wine["クラス"]))


result2 = clusGap(wine2, kmeans, K.max = 10, B = 100, verbose = interactive())
result2
plot(result2)

#clusterパッケージのインストール
install.packages("cluster")
library(cluster)

#距離を求める
dist = dist(wine2)^2
#シルエット係数を求める
sil = silhouette (result$cluster, dist)
plot(sil)

#mclust
#mclustパッケージのインストール
install.packages("mclust")
library(mclust)

wine.mclust = Mclust(wine2)
summary(wine.mclust)
wine.mclust$BIC
plot(wine.mclust)

