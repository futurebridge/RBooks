#シグモイド関数
★コマンド開始
#-5から5まで0.1ずつxに代入
x=seq(-5,5,0.1)
#シグモイド関数の結果をyに代入
y1 = 1 / (1+exp(-x))
#x,yをプロット
plot(x,y1,type="l",ylab="")
★コマンド終了

par(new=T)

#ReLu
y2 = ifelse(x>0,x,0)
plot(x,y2,type="l",lty=2, axes=FALSE,ylab="")
axis(4)

