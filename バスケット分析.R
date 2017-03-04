
install.packages("arules")
library(arules)

groceries=read.transactions(file='https://raw.githubusercontent.com/futurebridge/RBooks/master/groceries.csv',
                            sep=',')

summary(groceries)

inspect(groceries[1:10])

itemFrequencyPlot(groceries, topN=20)
itemFrequencyPlot(groceries, support=0.1)

#support 0.1 condidence=0.8
apriori(groceries)

groceryrules=apriori(groceries,parameter=list(support=0.05,confidence=0.05))

summary(groceryrules)

inspect(groceryrules)
