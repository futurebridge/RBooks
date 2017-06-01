

weather=read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/data.csv",header=TRUE)

h = hist(weather$min,main="ヒストグラム",xlab="最低気温",col="yellow")
n = length(h$counts) # クラスの数
class_names = NULL # クラスの名前

for(i in 1:n) {
  class_names[i] = paste(h$breaks[i], "～", h$breaks[i+1])
}

frequency_table = data.frame(クラス=class_names, 頻度=h$counts,相対比= h$counts/length(weather$min))
frequency_table

#正規分布
u = 171.9
sigma = 5.645
x = 180

1 /(sqrt(2*pi)*sigma)*exp(-((x-u)^2)/(2*(sigma^2)))
dnorm(x,u,sigma)

#t分布
#t分布による推定
bread=c(102,108,110,103,99,107,103,98,101,100)
bread
t.test(bread)

#指数の表示の抑制
10000000
options(scipen=10)
10000000



