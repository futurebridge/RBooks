###サポートベクターの計算
#kernlabパッケージをインストール
install.packages("kernlab")
#kernlabを有効にする
library(kernlab)


#値を用意する
x1 = scale(c(1,2,5,5,5))
x2 = scale(c(5,3,2,4,4))
y = c(+1,-1,-1,+1,+1)

svm = data.frame(x1,x2,y)
svm
svm.model = ksvm(factor(y)~x1+x2,data=svm,kernel="vanilladot")

#サポートベクターの要素を表示
svm.model@SVindex

#ラグランジュ乗数b、閾値bを表示
svm.model@b
svm.model@alpha

#ラグランジュ乗数αを表示、データフレーム型に代入
(alpha=data.frame(svm.model@alpha))

#βを求める
beta.x1=NULL
beta.x2=NULL
s=1
#サポートベクターを構成する要素についてx1,y1、αをかける
for (	i in svm.model@SVindex){
	beta.x1[s] = alpha[s,1] * x1[i] * y[i] 
	beta.x2[s] = alpha[s,1] * x2[i] * y[i] 
	s=s+1
}

#βを表示
sum(beta.x1)
sum(beta.x2)


#決定値の算出
decision=NULL
for(i  in 1:5){
decision[i]=sum(beta.x1) * x1[i] + sum(beta.x2) * x2[i] - svm.model@b
}
decision

#決定値の予測
predict(svm.model,svm,type="decision")



###カーネル関数を求める
weather=read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/data/data.csv",header=TRUE)

#それぞれの変数を正規化する
smax = scale(weather$max)
smin = scale(weather$min)

sigma1 = (2 * 1.5)^2 #σ=1.5
sigma2 = (2 * 5)^2   #σ=5
#-(x1 - x2)^2 / 2σ^2 を計算
kernel1 = (-(abs(smin-smax))^2) / sigma1
kernel2 = (-(abs(smin-smax))^2) / sigma2

summary(data.frame(kernel1,kernel2,exp(kernel1),exp(kernel2)))


###迷惑メール・正常メールデータのロード
library(kernlab)
data(spam)
set.seed(40)
#4601の中から任意に2500個を抽出
spam.num=sample(4601,2500)
spam.train=spam[spam.num,]
spam.test=spam[-spam.num,]

#線形分類でのモデル生成
spam.svm = ksvm(type~., data=spam.train, kernel="vanilladot")

spam.pre = predict(spam.svm, spam.test[,-58])

(spam.tab = table(spam.test[,58],spam.pre))
1-(sum(diag(spam.tab))/sum(spam.tab))



#カーネル法によるモデル生成
spam.svm = ksvm(type~., data=spam.train, kernel="rbfdot", C=5, kpar=list(sigma=10))

spam.pre = predict(spam.svm, spam.test[,-58])

(spam.tab = table(spam.test[,58],spam.pre))
1-(sum(diag(spam.tab))/sum(spam.tab))

#Cを５から１に下げてモデルを生成
spam.svm = ksvm(type~., data=spam.train, kernel="rbfdot", C=1, kpar=list(sigma=10))

spam.pre = predict(spam.svm, spam.test[,-58])

(spam.tab = table(spam.test[,58],spam.pre))
1-(sum(diag(spam.tab))/sum(spam.tab))


#σを10から0.1に下げてモデルを生成
spam.svm = ksvm(type~., data=spam.train, kernel="rbfdot", C=1, kpar=list(sigma=0.1))

spam.pre = predict(spam.svm, spam.test[,-58])

(spam.tab = table(spam.test[,58],spam.pre))
1-(sum(diag(spam.tab))/sum(spam.tab))

spam.pre=NULL
spam.svm=NULL	


#交差検証を20回実施
cross.svm = ksvm(type~., data=spam.train, kernel="rbfdot", cross=20)
spam.cpre = predict(cross.svm, spam.test[,-58])

(spam.tab = table(spam.test[,58],spam.cpre))
1-(sum(diag(spam.tab))/sum(spam.tab))
cross.svm
 

library(kernlab)


#アルファベットデータの読み込み
letters = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data",header=F)

colnames(letters)=c('letter','x-box','y-box','width','high','onpix','x-bar','y-bar','x2bar',
                 'y2bar','xybar','x2ybr','xy2br','x-ege','xegvy','y-ege','yegvx')


#letterAの先頭６データを表示
head(letters[letters$letter=="A",])

#データの先頭を表示
head(letters)
#データの行列
dim(letters)
#2万のデータのうち15000をランダムに抽出
letter.num=sample(20000,15000)
#15000をトレーニングデータに
letters_train=letters[letter.num,]
#残りの5000をテストデータに
letters_test=letters[-letter.num,]

#モデル作成・予測
letters.svm=ksvm(letter~.,data=letters_train,kernel="rbfdot",C=1, kpar=list(sigma=0.1))
letters.pre = predict(letters.svm,letters_test)

#結果の確認
head(letters.pre)
table(letters.pre,letters_test$letter)

#誤検知率の表示
#予測・正解データを行列型に代入
letters.mat=as.matrix(table(letters.pre,letters_test$letter))
#列名(A-Z)を取得
letters.col=colnames(letters.mat)
letters.name=NULL
accuracy=NULL

#アルファベット分、誤検知率を計算
for (i in 1:26){
 accuracy[i] = 1-letters.mat[i,i]/sum(letters.mat[,i])
 letters.name[i] =  paste(letters.col[i], 
                    "検知数",letters.mat[i,i],"合計",sum(letters.mat[,i]), 
                    " 誤検知率",round(1-letters.mat[i,i]/sum(letters.mat[,i]),3))
		
}
letters.name
mean(accuracy)

#1クラス　サポートベクターマシン
#ランダムなデータを用意する
x = rnorm(1000)
y = rnorm(1000)
cdata = data.frame(type=1, x, y)

#１クラスSVMモデルの作成
csvm = ksvm(type~.,data=cdata,type="one-svc",kernel="rbfdot",kpar=list(sigma=0.1),cross=10,nu=0.1)

#外れ値と正常値を予測する
svc.pre= predict(csvm)
#正常値を１、外れ値を２に分類する
d.svc.pre = ifelse(svc.pre==TRUE, 1, 2)

#元のx,yデータに正常値・異常値を付与する
data.result = cbind(cdata,d.svc.pre)
#正常値を赤、異常値を青としてプロットする
plot(data.result[,2:3], pch=21, bg=c("red","blue")[data.result$d.svc.pre])