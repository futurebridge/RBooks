#決定木のパッケージをインストール・有効にする
install.packages("rpart")
install.packages("rpart.plot")
library (rpart)
library(rpart.plot)

#集計データを個別データに変換
data(Titanic)
d.data = data.frame(Titanic)
d.data  =  data.frame(
  Class = rep(d.data$Class, d.data$Freq),
  Sex = rep(d.data$Sex, d.data$Freq),
  Age = rep(d.data$Age, d.data$Freq),
  Survived = rep(d.data$Survived, d.data$Freq)
)

rtree = rpart(Survived ~ Class + Sex + Age, data=d.data)
prp(rtree, type=1, extra=2)

#識別方法をジニ不純度として決定木を計算
rtree = rpart(Survived ~ Class + Sex + Age, 
data=d.data,parms=list(split="gini"))
#cpを表示・プロット
printcp(rtree)
plotcp(rtree)

#ランダムフォレスト
install.packages("randomForest")
library(randomForest)

library(kernlab)
data(spam)
set.seed(10)
tr.num=sample(4601,2500)
spam.train=spam[tr.num,]
spam.test=spam[-tr.num,] 


spam.rf = randomForest(type~., data=spam.train,na.action="na.omit")

head(spam.rf$err.rate)
tail(spam.rf$err.rate)
plot(spam.rf)


#重要度をプロット
varImpPlot(spam.rf) 
spam.rf$importance

#誤判定率を計測
spam.rfp　=  predict(spam.rf, spam.test[,-58])
(spam.rft = table(spam.test[,58], spam.rfp)) 
1-sum(diag(spam.rft))/sum(spam.rft) 




#アンサンブル学習パッケージadabagをインストール・有効
install.packages("adabag")
library(adabag)
library(kernlab)
data(spam)

set.seed(10)
tr.num = sample(4601,2500)
spam.train=spam[tr.num,]
spam.test=spam[-tr.num,]

spam.bag = bagging(type~.,data=spam.train,mfinal=15)

#各弱学習器の投票結果
head(spam.bag$votes)
#各弱学習器の確率
head(spam.bag$prob)

#決定木を表示
head(spam.bag$tree)

tail(sort(spam.bag$importance),20)
importanceplot(spam.bag)



#モデルの精度を予測する
spam.bagp=predict(spam.bag,spam.test[,-58],type= "class")

(spam.bagt=table(spam.test[,58], spam.bagp$class))
1-sum(diag(spam.bagt))/sum(spam.bagt)

#アダブースト
spam.bst = boosting(type~.,data=spam.train,mfinal=15,coeflearn="Zhu",control=rpart.control(maxdepth=5))
spam.bst

spam.bstp = predict.boosting(spam.bst, spam.test[,-58],type="class")
(spam.bagt=table(spam.test[,58], spam.bstp$class))
1-sum(diag(spam.bagt))/sum(spam.bagt)

evol.test=errorevol(spam.bst,newdata=spam.test)
evol.train=errorevol(spam.bst,newdata=spam.train)
plot.errorevol(evol.test,evol.train)



