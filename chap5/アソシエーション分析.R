
#arulesをインストール
install.packages("arules")
library(arules)

#groceriesファイルを読み込む
groceries=read.transactions(file='https://raw.githubusercontent.com/futurebridge/RBooks/master/chap5/groceries.csv',
                            sep=',')

summary(groceries)

#transactionデータ最初の５つを表示
inspect(groceries[1:5])

#購入頻度が多い上位２０アイテムを表示
itemFrequencyPlot(groceries, topN=20)

#support 0.1 condidence=0.8でアソシエーションルール抽出
grule1=apriori(groceries)

#support 0.05 condidence=0.1 でアソシエーションルール抽出
grule2=apriori(groceries,parameter=list(support=0.05,confidence=0.1))
inspect(grule2)
#リフト値1.1以上を抽出
grule3 = subset(grule2,subset=(lift>=1.1))


#support 0.03 condidence=0.03 でアソシエーションルール抽出
grule2=apriori(groceries,parameter=list(support=0.03,confidence=0.03))
grule3 = subset(grule2,subset=(lift>=1.1))
inspect(grule3)

#arulesVizのインストール
install.packages(arulesViz)
library(arulesViz)
#グラフの表示
plot(grule3,method="graph")
#インタラクティブモード
plot(grule3,method="graph",interactive=TRUE)


#偶数だけを抽出しgrule4に代入
grule4=grule3[seq(0, length(grule3), +2),]
#クラスタリング
d=dissimilarity(grule3)

#それぞれのクラスタリング手法でクラスタリング
sngl=hclust(d,"single")
comp=hclust(d) 
aver=hclust(d,"average")
ward=hclust(d,"ward.D2")
cntr=hclust(d,"centroid")
medi=hclust(d,"median")

#グラフの描画
par(mfrow=c(2,3));
plot(sngl, main="単連結法"); 
plot(comp, main="完全連結法"); 
plot(aver, main="群平均法") ;　
plot(ward, main="ウォード法"); 
plot(cntr, main="重心法"); 
plot(medi, main="メディアン法");

#ルールを表示
inspect(grule3)

