
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

#逸脱度を表示
anova(result)

