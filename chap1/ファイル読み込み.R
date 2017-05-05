#同じディレクトリからファイルを読み込む
weather = read.csv(“data.csv”,header=TRUE)

＃URLからファイルを読み込む
weather=read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/data/data.csv",header=TRUE)

#weatherの表示
weather

#現在のディレクトリを表示
getwd()
