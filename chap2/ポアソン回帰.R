

#ポアソン分布を求める
dpois(6,4)

#ポアソン回帰に必要なデータ(faraway)パッケージのインストール
install.packages("faraway")
library(faraway)
data(gala)
summary(gala)

gala.pm = glm(Species~ ., data = gala, family = poisson)
summary(gala.pm)

gala.pm2 = step(gala.pm)
summary(gala.pm2)

#モデルをもとに予測する
predict(gala.pm2,type="respon")
#実測値と比較
data.frame(gala$Species,round(gala.pre,3),gala$Species-round(gala.pre,3))
