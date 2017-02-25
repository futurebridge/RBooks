beer.d = dist(beer)
beer.d
beer.d2 = beer.d ^ 2

sngl=hclust(beer.d,"single")
comp=hclust(beer.d) 
aver=hclust(beer.d,"average")
ward=hclust(beer.d,"ward.D2")
cntr=hclust(beer.d,"centroid")
medi=hclust(beer.d,"median")

par(mfrow=c(2,3));
plot(sngl, main="単連結法"); 
plot(comp, main="完全連結法"); 
plot(aver, main="群平均法") ;　
plot(ward, main="ウォード法"); 
plot(cntr, main="重心法"); 
plot(medi, main="メディアン法");

data.frame(sngl$merge,comp$merge,aver$merge,ward$merge,cntr$merge,medi$merge)

