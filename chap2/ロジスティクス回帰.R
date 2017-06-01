
##シグモイド関数
#-5から5まで、0.1ずつxに代入
x=seq(-5,5,0.1)
#シグモイド関数の結果をyに代入
y = 1 / (1+exp(-x))
#x,yをプロット
plot(x,y)

##ロジスティクス回帰１
#データ読み込み　タイタニック乗員データ
d.data = data.frame(Titanic)

#Class, Sex, Age ,Survived にFreq(頻度）を代入

d.data  =  data.frame(
  Class = rep(d.data$Class, d.data$Freq),
  Sex = rep(d.data$Sex, d.data$Freq),
  Age = rep(d.data$Age, d.data$Freq),
  Survived = rep(d.data$Survived, d.data$Freq)
)

summary(d.data)
head(d.data)

#目的変数 Survived (Yes/No) 説明変数を Class, Sex, Age
result = glm(Survived~., data = d.data, family = binomial)

summary(result)

#逸脱度を表示
anova(result,test="Chisq")

#係数を表示
result$coefficients
#eに係数乗＝オッズ比
exp(result$coefficients)


##ロジスティクス回帰2 迷惑メール判定
#kernlabパッケージの導入
install.packages("kernlab")
library(kernlab)
#spamデータのロード
data(spam)
#行と列の数を表示
dim(spam)
#58列目を集計
table(spam[,58])

#0～4601までのランダムな値を生成
tr.num=sample(4601,2500)
#spamデータからランダムに選んだ値をモデル学習データに充てる
spam.train=spam[tr.num,]
#モデル学習データ以外の残り(-tr.num)をテストデータに充てる
spam.test=spam[-tr.num,]

#行と列の数を表示
dim(spam.train)
dim(spam.test)

#モデル作成
glm.model = glm(type~., spam.train, family=binomial)
summary(glm.model)

sort(round(exp(glm.model$coefficients),3))

glm.model2 = update(glm.model, ~ . - cs, data = spam.train)

glm.pre = ifelse(predict(glm.model,spam.test[,-58]) > 0,"spam","nospam")
glm.pre
(glm.tab = table(spam.test[ ,58], glm.pre))
##ロジスティクス回帰2 迷惑メール判定
#kernlabパッケージの導入
install.packages("kernlab")
library(kernlab)
#spamデータのロード
data(spam)
#行と列の数を表示
dim(spam)
#58列目を集計
table(spam[,58])

#0～4601までのランダムな値を生成
tr.num=sample(4601,2500)
#spamデータからランダムに選んだ値をモデル学習データに充てる
spam.train=spam[tr.num,]
#モデル学習データ以外の残り(-tr.num)をテストデータに充てる
spam.test=spam[-tr.num,]

#行と列の数を表示
dim(spam.train)
dim(spam.test)

#モデル作成
glm.model = glm(type~., spam.train, family=binomial)
summary(glm.model)

#テストデータを代入し、0以上であればspam, それ以外であればnonspam
glm.pre = ifelse(predict(glm.model,spam.test[,-58]) > 0,"spam","nonspam")
glm.pre
(glm.tab = table(spam.test[ ,58], glm.pre))
1-(sum(diag(glm.tab))/sum(glm.tab))

#AICでパラメータを削減
glm.model2 = step(glm.model)

#AIC実行前と後でのＡＩＣ，パラメータ数を表示
glm.model$aic
glm.model2$aic
length(glm.model$coefficients)
length(glm.model2$coefficients)

#AIC実行後の誤検知率を表示
glm.pre = ifelse(predict(glm.model2,spam.test[,-58]) > 0,"spam","nonspam")
(glm.tab = table(spam.test[ ,58], glm.pre))
1-(sum(diag(glm.tab))/sum(glm.tab))




