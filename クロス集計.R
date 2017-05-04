#dplyrパッケージのインストール
install.packages("dplyr")
#dplyrパッケージの有効化
library(dplyr)
#csvファイルの読み込み
kakeibo = read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/family.csv", header=TRUE)
#dplyrデータ形式に変換
kakeibo.d = tbl_df(kakeibo)
#支払先をもとに金額を集計
summarise(group_by(kakeibo.d, 支払先), 合計=sum(金額))
#支出科目をもとに金額を集計
summarise(group_by(kakeibo.d, 支出科目), 合計=sum(金額))
