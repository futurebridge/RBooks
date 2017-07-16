#ビールデータの読み込み
source("https://raw.githubusercontent.com/futurebridge/RBooks/master/chap3/beer.R")
beer

#相関行列を求める ()は変数に代入しながら値を表示する
(Rbeer = cor(beer))

#相関行列から固有値、固有ベクトルを求める
(Ebeer = eigen(Rbeer))


#累積寄与率を求める
contribution=NULL
accumulate=0
i=0
for (i in 1:3){
	accumulate= Ebeer$values[i]/sum(Ebeer$values)+accumulate
	contribution[i] =　paste ("第",i,"主成分","固有値",Ebeer$values[i]
                                ,"寄与率",Ebeer$values[i]/sum(Ebeer$values)
					  ,"累積寄与率",accumulate)
}
contribution

#第1、第2主成分の固有ベクトルをプロットする
plot(Ebeer$vectors[1:3,1:2],xlab="第1主成分",ylab="第2主成分")



#主成分分析
result = prcomp(beer, scale=TRUE)
#各ビールの主成分得点を表示
result$x

biplot(result)

#ワインデータを読み込む
wine = read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data')

colnames(wine)=c('クラス','アルコール','リンゴ酸','灰','灰のアルカリ度','マグネシウム','フェノール','フラボノイド','ノンフラボノイド',
                 'ポリフェノール','色','色相','OD280/OD315','プロリン')

wine2 = wine[setdiff(colnames(wine), "クラス")]

#固有値固有ベクトルを求める
Rwine = cor(wine2)
Ewine = eigen(Rwine)
Ewine$values

#累積寄与率を求める
contribution=NULL
accumulate=0
i=0

for (i in 1:13){
	accumulate= Ewine$values[i]/sum(Ewine$values)+accumulate
	contribution[i] =　paste ("第",i,"主成分","固有値",Ewine$values[i]
                                ,"寄与率",Ewine$values[i]/sum(Ewine$values)
					  ,"累積寄与率",accumulate)
}
contribution



result.wine = prcomp(wine2, scale=TRUE)

biplot(result.wine)

#第1主成分得点（PC1）について降順にソートしてdataframe型で表示する
data.frame(sort(result.wine$rotation[,1]))

#第2主成分得点（PC1）について降順にソートしてdataframe型で表示する
data.frame(sort(result.wine$rotation[,2]))

