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



l2model = glmnet(as.matrix(spam[, -58]), spam[, 58], family = "binomial", alpha=0)

cvmodel = cv.glmnet(as.matrix(spam[, -58]), spam[, 58], family = "binomial")




par (mfrow=c(1,3))
plot(l2model)
plot(cvmodel)

