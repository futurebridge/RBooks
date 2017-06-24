#glmnetのインストール・有効

install.packages("glmnet")
library(glmnet)

#spamデータのロード
library(kernlab)
data(spam)

#L1正則化による推定
l1model = glmnet(as.matrix(spam[, -58]), spam[, 58], family = "binomial", alpha=1)
plot(l1model,label=TRUE,xvar="lambda")

#説明変数の係数を表示
glm.model = glm(type~., spam.train, family=binomial)
sort(glm.model$coefficients)
#最も係数が低い41列の要素を表示
colnames(spam[41])
#最も係数が高い53列の要素を表示
colnames(spam[53])

#L2正則化による推定
l2model = glmnet(as.matrix(spam[, -58]), spam[, 58], family = "binomial", alpha=0)
plot(l2model,label=TRUE,xvar="lambda")

#重要度を求める
dotchart(tail(log(sort(l2model$beta[,1])),30),main="importance")

#λを求める
cvmodel = cv.glmnet(as.matrix(spam[, -58]), spam[, 58], family = "binomial")
plot(cvmodel)

#二乗誤差の最小値
min(cvmodel$cvm)
#λの最小値
cvmodel$lambda.min
#λの最小値を対数変換
log(cvmodel$lambda.min)

