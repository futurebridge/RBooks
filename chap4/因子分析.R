gakuryoku = read.csv("https://raw.githubusercontent.com/futurebridge/RBooks/master/chap4/gakuryoku.csv",header=TRUE)

Rgakuryoku=cor(gakuryoku)
Egakuryoku=eigen(Rgakuryoku)
Egakuryoku

#—İÏŠñ—^—¦‚ğ‹‚ß‚é
contribution=NULL
accumulate=0
i=0

for (i in 1:6){
	accumulate= Egakuryoku$values[i]/sum(Egakuryoku$values)+accumulate
	contribution[i] =@paste ("‘æ",i,"å¬•ª","ŒÅ—L’l",Egakuryoku$values[i]
                                ,"Šñ—^—¦",Egakuryoku$values[i]/sum(Egakuryoku$values)
					  ,"—İÏŠñ—^—¦",accumulate)
}
contribution




result = prcomp(gakuryoku,scale=TRUE)
biplot(result)

#ˆöq•ªÍF‰ñ“]‚È‚µ
result.none = factanal(gakuryoku,factors=2,rotation="none")

#ˆöq•ªÍF’¼Œğ‰ñ“]
result.vari = factanal(gakuryoku,factors=2,rotation="varimax")

#ˆöq•ªÍFÎŒğ‰ñ“]
result.pro = factanal(gakuryoku,factors=2,rotation="promax")

result.none
result.vari
result.pro





