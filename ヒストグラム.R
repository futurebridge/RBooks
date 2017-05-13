

weather=read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/data.csv",header=TRUE)

h = hist(weather$min,main="ヒストグラム",xlab="最低気温",col="yellow")
n = length(h$counts) # クラスの数
class_names = NULL # クラスの名前

for(i in 1:n) {
  class_names[i] = paste(h$breaks[i], "～", h$breaks[i+1])
}

frequency_table = data.frame(クラス=class_names, 頻度=h$counts,相対比= h$counts/length(weather$min))
frequency_table


h1 = hist(weather$min,main="ヒストグラム",xlab="最低気温",col="yellow",breaks=10)
sum (h1$counts / length(weather$min))
h2= hist(weather$min,main="ヒストグラム",xlab="最低気温",col="yellow",breaks=30)
sum (h2$counts / length(weather$min))

#正規分布
u = 171.9
sigma = 5.645
x = 180

1 /(sqrt(2*pi)*sigma)*exp(-((x-u)^2)/(2*(sigma^2)))
dnorm(x,u,sigma)