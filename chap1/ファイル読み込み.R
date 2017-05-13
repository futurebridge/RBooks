#同じディレクトリからファイルを読み込む
weather = read.csv(“data.csv”,header=TRUE)

＃URLからファイルを読み込む
weather=read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/data/data.csv",header=TRUE)

#weatherの表示
weather

#現在のディレクトリを表示
getwd()

#変数を表示
weather
#変数の各要素を法事
weather$min
#変数の概要を表示
summary(weather$min)

