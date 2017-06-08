children = read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/chap2/children.csv", header=TRUE)
summary(children)

#NAについて削除
children=na.omit(children)


#年齢と体重との相関係数を算出
cor.test(children$age,children$weight)
#年齢と身長との相関係数を算出
cor.test(children$age,children$length)

＃２ｘ２のグラフを出力　最後に”;”をつける
par(mfrow=c(2,2));
＃年齢と身長のグラフを描画
plot(children$age, children$length,main="年齢と身長",ylab="年齢",xlab="身長")
＃年齢と体重のグラフを描画
plot(children$age, children$weight,main="年齢と体重",ylab="年齢",xlab="体重")

#身長の分布に関するヒストグラムを描画
hist(children$length,main="身長の分布",xlab="身長")
#身長の分布に関するヒストグラムを描画
hist(children$weight,main="体重の分布",xlab="身長")

#回帰モデルの作成
result = lm(age~length, data=children)
summary(result)

#モデルを元に予測
lmpredict = predict(result)
lmresiduals  = residuals(result)

#予測値と残差の分布をプロット
par(mfrow=c(1,3));
plot(lmpredict,children$age,main="年齢の推定値と実測値",ylab="推定値",xlab="実測値")
hist(lmpredict, main="予測値分布",xlab="予測値",col = "#ff00ff40", border = "#ff00ff")
hist(lmresiduals , main="残差分布",xlab="残差",col = "#0000ff40", border = "#0000ff")

#残差をプロット
par (mfrow=c(2,2)) 
plot(result)

#クック距離を求める
cooks.distance(result)
#年齢・身長の実測値と結合
ck.d=data.frame(children$age,children$length,cooks.distance(result))
#クック距離0.02以上を抽出
subset(ck.d,ck.d$cooks.distance.result>0.02)


#予測区間を求める
lm.predict = predict(result, interval="prediction")
#信頼区間を求める
lm.confidence = predict(result,interval="confidence")

summary(lm.predict)
summary(lm.confidence)


##重回帰分析
children = read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/chap2/children2.csv",header=TRUE)
#NAについて削除
children=na.omit(children)
cor (children) #相関行列を表示


pairs(children) #散布図を作成

lmresult = lm(age~length+weight+tenaga+tekubi, data=children)

summary(lmresult)

#car パッケージのインストール
install.packages("car")
library(car)
vif(lmresult)

#多重共線性を回避するため、説明変数を減らしてモデルを作成
lmresult = lm(age~weight+tenaga+tekubi, data=children)
vif(lmresult)

#AIC.Rを読み込む
source("https://raw.githubusercontent.com/futurebridge/RBooks/master/chap2/AIC.R")
#変数を表示
aictest
#相関行列の表示
cor(aictest)

#MASSライブラリを有効にする
library(MASS)

#重回帰モデルの作成
result=lm(Y~X1+X2+X3+X4,data=aictest)
#AICの実行
result2 = stepAIC(result)

　




