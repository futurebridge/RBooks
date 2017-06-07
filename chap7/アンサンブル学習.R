#決定木のパッケージをインストールして有効にする
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

#変換する前のデータの表示(最後の10行)
tail(data.frame(Titanic,10))
#変換した後のデータの表示(最後の10行)
tail(data.frame(d.data,10))

#決定木の構築
rtree = rpart(Survived ~ Class + Sex + Age, data=d.data)
#決定木のプロット
prp(rtree, type=1, extra=2)

#識別方法としてジニ不純度（gini）を指定して決定木を計算
rtree = rpart(Survived ~ Class + Sex + Age, 
data=d.data,parms=list(split="gini"))
#cpをプロット
plotcp(rtree)

#アンサンブル学習パッケージadabagをインストール・有効にする
install.packages("adabag")
library(adabag)
library(kernlab)
data(spam)


set.seed(10) #結果の再現性を確保するため同じ乱数を発生させる
tr.num = sample(4601,2500)
spam.train=spam[tr.num,]
spam.test=spam[-tr.num,]

#弱学習器15個でバギングを実施
spam.bag = bagging(type~.,data=spam.train,mfinal=15)



#各弱学習器の投票結果・確率・クラスの表示（最後の30行）
tail(data.frame(spam.bag$votes,spam.bag$prob,spam.bag$class),30)

#決定木の表示
head(spam.bag$tree)

#重要度上位30位を表示
tail(sort(spam.bag$importance),30)
importanceplot(spam.bag)

#モデルの精度を予測する
spam.bagp=predict(spam.bag,spam.test[,-58],type= "class")

(spam.bagt=table(spam.test[,58], spam.bagp$class))
1-sum(diag(spam.bagt))/sum(spam.bagt)


#アダブースト
spam.bst = boosting(type~.,data=spam.train,mfinal=15)
spam.bst

#各弱学習器の投票結果・確率・クラスの表示（最後の30行）
tail(data.frame(spam.bst$votes,spam.bst$prob,spam.bst$class),30)

#認識率の算出
spam.bstp = predict.boosting(spam.bst, spam.test[,-58],type="class")
(spam.bagt=table(spam.test[,58], spam.bstp$class))
1-sum(diag(spam.bagt))/sum(spam.bagt)


#ブースティング、バギングのエラー率を求める
bst.test=errorevol(spam.bst,newdata=spam.test)
bst.train=errorevol(spam.bst,newdata=spam.train)
bag.test=errorevol(spam.bag,newdata=spam.test)
bag.train=errorevol(spam.bag,newdata=spam.train)

#求めたエラー率をプロットする
par(mfrow=c(1,2));
plot.errorevol(bst.test,bst.train,sub="ブースティング")
plot.errorevol(bag.test,bag.train,sub="バギング")


#ランダムフォレストの導入
install.packages("randomForest")
library(randomForest)

#データを準備する
library(kernlab)
data(spam)
set.seed(10)
tr.num=sample(4601,2500)
spam.train=spam[tr.num,]
spam.test=spam[-tr.num,] 

#ランダムフォレストのモデルを作成
spam.rf = randomForest(type~., data=spam.train,na.action="na.omit")
head(spam.rf$err.rate)
tail(spam.rf$err.rate)
plot(spam.rf)

#説明変数の重要度を表示
varImpPlot(spam.rf) 
spam.rf$importance


#誤検知率を計測
spam.rfp　=  predict(spam.rf, spam.test[,-58])
(spam.rft = table(spam.test[,58], spam.rfp)) 
1-sum(diag(spam.rft))/sum(spam.rft) 
