## Association Rules Groceries

library(arules)
library(arulesViz)

groceries_data <- read.csv("C:\\PRATIK\\Data Science\\Assignment\\Association\\groceries.csv")

str(groceries_data)

install.packages("mvinfluence",repos = "http://cran.us.r-project.org")
library(mvinfluence)

library(MASS)

library(caret)

#Changing the data from to transactions
groceries <- as(groceries_data, "transactions")

itemFrequencyPlot(groceries, topN=20)

grocires_apriori <- apriori(groceries, parameter = list(supp=0.005, conf=0.45, minlen=2, maxlen=4))

grocires_apriori

#Plotting the data rules
inspect(head(sort(grocires_apriori),n=20))

inspect(tail(sort(grocires_apriori),n=20))

plot(head(sort(grocires_apriori),n=20), method="graph", control=list(cex=0.70))

plot(grocires_apriori)

plot(head(sort(grocires_apriori),n=10), method="grouped", control=list(cex=0.2))
