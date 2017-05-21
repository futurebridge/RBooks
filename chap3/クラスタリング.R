#ユークリッド距離を求める
国語 = c(80,70,90)
数学 = c(60,40,30)

点数 = data.frame(国語,数学,row.names=c("田中さん","佐藤さん","鈴木さん"))
点数
dist(点数)

#ビールデータの読み込み
source("https://raw.githubusercontent.com/futurebridge/RBooks/master/chap3/beer.R")
beer

beer.d = dist(beer)
round(beer.d,2)

#クラスタリングを実施
sngl=hclust(beer.d,"single")
#併合過程を表示
sngl$merge



comp=hclust(beer.d) 
aver=hclust(beer.d,"average")
ward=hclust(beer.d,"ward.D2")
cntr=hclust(beer.d,"centroid")
medi=hclust(beer.d,"median")

label.d=c("単連結法","単連結法","完全連結法","完全連結法","群平均法","群平均法","ウォード法","ウォード法","重心法","重心法","メディアン法","メディアン法")
merge.d=data.frame(sngl$merge,comp$merge,aver$merge,ward$merge,cntr$merge,medi$merge)
names(merge.d)=label.d
merge.d



par(mfrow=c(2,3));
plot(sngl, main="単連結法"); 
plot(comp, main="完全連結法"); 
plot(aver, main="群平均法") ;　
plot(ward, main="ウォード法"); 
plot(cntr, main="重心法"); 
plot(medi, main="メディアン法");

