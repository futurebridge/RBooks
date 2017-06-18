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
#SGDパッケージのインストール・導入
install.packages("sgd")
library(sgd)

#乱数を設定する
set.seed(42)
#10000個のデータを作成
N = 10000
#特定するパラメータ数
d = 10
#10000個のデータに乱数を代入
X = matrix(rnorm(N*d), ncol=d)
#特定する目的変数を設定（５を１０個）
theta = rep(5, d+1)
eps = rnorm(N)
y = cbind(1, X) %*% theta + eps
dat = data.frame(y=y, x=X)
#回帰モデルでSGDの計算
sgd.theta = sgd(y ~ ., data=dat, model="lm")

#SGD計算が収束したかどうか（TRUE:収束、FALSE：発散）
sgd.theta$converged

#SDGによる推定結果
sgd.theta$coefficients

#二乗誤差を計算
sprintf("平均二乗誤差: %0.3f", mean((theta - as.numeric(sgd.theta$coefficients))^2))


#H2Oパッケージのインストール
# すでにh2oパッケージがインストールされていたら削除する
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# h2依存パッケージをダウンロード
if (! ("methods" %in% rownames(installed.packages()))) { install.packages("methods") }
if (! ("statmod" %in% rownames(installed.packages()))) { install.packages("statmod") }
if (! ("stats" %in% rownames(installed.packages()))) { install.packages("stats") }
if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
if (! ("RCurl" %in% rownames(installed.packages()))) { install.packages("RCurl") }
if (! ("jsonlite" %in% rownames(installed.packages()))) { install.packages("jsonlite") }
if (! ("tools" %in% rownames(installed.packages()))) { install.packages("tools") }
if (! ("utils" %in% rownames(installed.packages()))) { install.packages("utils") }

#h2o パッケージの導入
#ディレクトリが変わる可能性があるので、インストールできない場合は、　https://www.h2o.ai/download/　を参照
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-ueno/8/R")))
library(h2o)

localH2O = h2o.init(nthreads=-1)

library(kernlab)
data(spam)
set.seed(50)

spam.num=sample(4601,2500)
spam.train=spam[spam.num,]
spam.test=spam[-spam.num,]

model = h2o.deeplearning(x = 1:57, y = 58, training_frame = as.h2o(spam.train),activation="RectifierWithDropout",hidden=c(20, 20, 20),epochs = 100)

model

#テストデータをもとに予測
dp.pre = h2o.predict(object=model,newdata = as.h2o(spam.test[,-58]))
#結果を表示
dp.pre
#h2oらデータフレーム型に変換
dp.result = as.data.frame(dp.pre)
#実際の正解データとの比較
(spam.tab = table(spam.test[,58],dp.result$predict))
1-(sum(diag(spam.tab))/sum(spam.tab))



