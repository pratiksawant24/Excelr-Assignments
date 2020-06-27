## Association Rules Groceries

#install.packages("rmarkdown",repos = "http://cran.us.r-project.org")
#install.packages("arules",repos = "http://cran.us.r-project.org")
#install.packages("arulesViz",repos = "http://cran.us.r-project.org")

library(arules)
library(rmarkdown)
library(arulesViz)

data()
data("Groceries")
summary(Groceries)

rules <- apriori(Groceries,parameter=list(support=0.002, confidence = 0.5))

rules

inspect(head(sort(rules, by = "lift")))

plot(rules)

head(quality(rules))

plot(rules, method = "grouped")   #Grouped Matrix for 1098 Rules 

plot(rules,method = "scatterplot")   #Scatterplot for 1098 Rules

plot(rules,method = "grouped")   #Grouped Matrix for 1098 Rules

plot(rules,method = "graph")   #Graph for 100 Rules
