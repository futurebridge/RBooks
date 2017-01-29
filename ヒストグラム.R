
h = hist(weather$min,main="ヒストグラム",xlab="最低気温",col="#edae00")
n = length(h$counts) # クラスの数
class_names = NULL # クラスの名前
frequency_table =NULL
accumulate=0 #相対比
for(i in 1:n) {
  class_names[i] = paste(h$breaks[i], "～", h$breaks[i+1])
}

frequency_table = data.frame(クラス=class_names, 頻度=h$counts,相対比= h$counts/length(weather$min))
frequency_table


