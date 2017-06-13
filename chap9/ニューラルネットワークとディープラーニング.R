#-5から5まで0.1ずつxに代入
x=seq(-5,5,0.1)

#シグモイド関数
y1 = 1 / (1+exp(-x))
#x,yをプロット
plot(x,y1,type="l",ylab="",ylim=c(-1,5))
par(new=T)

#tanh(双曲線正接関数）
y2 = (exp(x)-exp(-x))/(exp(x)+exp(-x))
par(new=T)
plot(x,y2,type="l",col="red",lty=3,ylab="",ylim=c(-1,5))
par(new=T)

#ReLu(正規化線形関数）
#0以上であればx,それ以外であれば0
y3 = ifelse(x>0,x,0)
plot(x,y3,type="l",col="blue",lty=4,ylab="",ylim=c(-1,5))

#多層パーセプトロンの計算
#入力x
(x1=matrix(c(0.6,2.1,3.2),1,3))
#重み
(w1=matrix(c(0.2,0.4,0.3,1.2,0.6,0.3),3,2))
b = 2 
(a = x1 %*% w1 + b)

#ニューラルネット
concrete = read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/chap9/concrete.csv") 
str(concrete)

normalize = function(x){
 return((x-min(x)) / (max(x)-min(x)))
}

#コンクリートデータを正規化(normalize関数を適用)
concrete_norm = as.data.frame(lapply(concrete,normalize))

#トレーニングデータとテストデータに分ける
concrete_train = concrete_norm[1:733,]
concrete_test = concrete_norm[774:1030,]

head(concrete)
head(concrete_norm)

#neuralnetパッケージをインストール
install.packages("neuralnet")
library(neuralnet)

#ニューラルネットモデルを作成
concrete_model = neuralnet(strength ~ cement + slag + ash+ water+superplastic+coarseagg+fineagg+age,data=concrete_train,hidden=3)
plot(concrete_model)

#モデルをもとに結果を予測
model_result = compute(concrete_model,concrete_test[1:8])
predicted_strength = model_result$net.result

#実測値との比較
cor(predicted_strength, concrete_test$strength)


#微分
x = seq(-5, 5, 0.05)
D.fx = function(x) {
     
    h = 0.000001
    y = ((x + h)^2 - (x^2)) / h
     
    return(y)
}

plot(x, x^2, type = "l", xlim = c(-5, 5), ylim = c(-6, 9), ylab = "")
par(new = T)
plot(x, D.fx(x), type = "l", xlim = c(-5, 5), ylim = c(-6, 9), ylab = "")


#SGD
install.packages("sgd")
library(sgd)

concrete = read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/concrete.csv") 
str(concrete)

normalize = function(x){
 return((x-min(x)) / (max(x)-min(x)))
}

#コンクリートデータを正規化
concrete_norm = as.data.frame(lapply(concrete,normalize))

#トレーニングデータとテストデータに分ける
concrete_train = concrete_norm[1:733,]
concrete_test = concrete_norm[774:1030,]

head(concrete)
head(concrete_norm)

sgd.theta = sgd(strength ~ ., data=concrete_norm,
               model="glm", model.control=binomial(link="logit"),
               sgd.control=list(reltol=1e-5, npasses=100),
                 lr.control=c(scale=1, gamma=1, alpha=30, c=1))
sgd.theta


#H2Oパッケージのインストール
install.packages("h2o",type="source",repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-ueno/6/R")))
library(h2o)
localH2O = h2o.init(nthreads=-1)


library(kernlab)
data(spam)
set.seed(50)

spam.num=sample(4601,2500)
spam.train=spam[spam.num,]
spam.test=spam[-spam.num,]

model = h2o.deeplearning(x = 1:57, y = 58, training_frame = as.h2o(spam.train),activation="RectifierWithDropout",hidden=c(20, 20, 20),epochs = 1000)

#テストデータをもとに予測
dp.pre = h2o.predict(object=model,newdata = as.h2o(spam.test[,-58]))
#結果を表示
dp.pre
#h2oらデータフレーム型に変換
dp.result = as.data.frame(dp.pre)
#実際の正解データとの比較
(spam.tab = table(spam.test[,58],dp.result$predict))
1-(sum(diag(spam.tab))/sum(spam.tab))



