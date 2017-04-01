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



