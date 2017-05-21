

beer.d = dist(beer)
round(beer.d,2)

sngl=hclust(beer.d,"single")
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

