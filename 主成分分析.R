#ビールデータの読み込み
source("https://raw.githubusercontent.com/futurebridge/RBooks/master/beer.R")
beer

#相関行列を求める ()は値を表示する
(Rbeer = cor(beer))
#参考までに分散共分散行列を求める
cov(beer)

#相関行列から固有値、固有ベクトルを求める
(Ebeer = eigen(Rbeer))

#累積寄与率を求める
contribution=NULL
accumulate=0
for (i in 1:3){
	accumulate= round(Ebeer$values[i]/sum(Ebeer$values),3)+accumulate
	contribution[i] =　paste ("第",i,"主成分","固有値",round(Ebeer$values[i],3)
                                ,"寄与率",round(Ebeer$values[i]/sum(Ebeer$values),3)
					  ,"累積寄与率",accumulate)
}
contribution

#第1、第2主成分の固有ベクトルをプロットする
Ebeer



