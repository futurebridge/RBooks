
#カーネル関数を求める
weather=read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/data.csv",header=TRUE)

#それぞれの変数を正規化する
smax = scale(weather$max)
smin = scale(weather$min)

sigma1 = (2 * 1.5)^2 #σ=1.5
sigma2 = (2 * 5)^2   #σ=5
#-(x1 - x2)^2 / 2σ^2 を計算
kernel1 = (-(abs(smin-smax))^2) / sigma1
kernel2 = (-(abs(smin-smax))^2) / sigma2

summary(data.frame(kernel1,kernel2))


#迷惑メール・正常メールデータのロード
library(kernlab)
data(spam)
#4601の中から任意に2500
spam.num=sample(4601,2500)


letters = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data",header=F)

colnames(letters)=c('letter','x-box','y-box','width','high','onpix','x-bar','y-bar','x2bar',
                 'y2bar','xybar','x2ybr','xy2br','x-ege','xegvy','y-ege','yegvx')


letters2 = letters[setdiff(colnames(letters), "letter")]

dim(letters)
letter.num=sample(20000,15000)
letter.num

letters_train=letter[letter.num,]
letters_test=letter[-letter.num,]

library(kernlab)

letters.svm=ksvm(letter~.,data=letters_train,kernel="rbfdot")
letters.pre = predict(letter.svm,letters_test)

head(letters.pre)

table(letters.pre,letters_test$letter)