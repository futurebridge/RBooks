children = read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/children.csv", header=TRUE)

summary(children)

#年齢と体重との相関係数を算出
cor.test(children$age,children$weight)
#年齢と身長との相関係数を算出
cor.test(children$age,children$length)

＃２ｘ２のグラフを出力　最後に”;”をつける
par(mfrow=c(2,2));
＃年齢と身長のグラフを描画
plot(children$age, children$length)
＃年齢と体重のグラフを描画
plot(children$age, children$weight)

#身長の分布に関するヒストグラムを描画
hist(children$length)
#身長の分布に関するヒストグラムを描画
hist(children$weight)

#年齢のデータ個数を表示
sum(table(children$age))

#身長のデータ個数を表示
sum(table(children$age))

#体重のデータ個数を表示
sum(table(children$age))


result = lm(age~length, data=children)
summary(result)



#F検定
var.test(children$age, children$length)

lmpredict = predict(result)
lmresiduals  = residuals(result)

par(mfrow=c(1,2));
hist(lmpredict, main="予測値分布",col = "#ff00ff40", border = "#ff00ff")
hist(lmresiduals , main="残差分布",col = "#0000ff40", border = "#0000ff")


par (mfrow=c(2,2)) 
plot(lmresult)

lm.predict = predict(result, interval="prediction")
lm.confidence=　predict(result,interval="confidence")


##重回帰分析
children = read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/children2.csv",header=TRUE)
children[is.na(children)] = 0 #欠損値に0を代入
cor (children) #相関行列を表示

pairs(children) #散布図を作成

lmresult = lm(age~length+weight+tenaga+tekubi, data=children)

summary(lmresult)

#car パッケージのインストール
install.packages("car")
library(car)
vif(lmresult)


#F検定
var.test(children$length+children$weight+children$tenaga+children$tekubi,children$age)




