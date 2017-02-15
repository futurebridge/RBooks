install.packages("faraway")
library(faraway)
data(gala)
summary(gala)

gala.pm = glm(Species~ ., data = gala, family = poisson)
summary(gala.pm)

gala.pm2 = step(gala.pm)
summary(gala.pm2)

#ƒ‚ƒfƒ‹‚ð‚à‚Æ‚É—\‘ª‚·‚é
predict(gala.pm2,type="respon")
#ŽÀ‘ª’l‚Æ”äŠr
data.frame(gala$Species,round(gala.pre,3),gala$Species-round(gala.pre,3))
