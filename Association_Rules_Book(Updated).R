##Association Rules Book

#install.packages("rmarkdown",repos = "http://cran.us.r-project.org")
#install.packages("arules",repos = "http://cran.us.r-project.org")
#install.packages("arulesViz",repos = "http://cran.us.r-project.org")

library(arules)
library(rmarkdown)
library(arulesViz)

book <- read.csv(file.choose())

View(book)

rules <- apriori(as.matrix(book),parameter=list(support=0.02, confidence = 0.5,minlen=5))# Provided the rules with 2 % Support, 50 % Confidence and Minimum to purchase 5 books 

rules

inspect(head(sort(rules, by = "lift"))) 

head(quality(rules))

plot(rules,method = "scatterplot")

plot(rules,method = "grouped")

plot(rules,method = "graph")
