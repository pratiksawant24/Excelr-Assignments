##Association Rule Movies

install.packages("rmarkdown",repos = "http://cran.us.r-project.org")

install.packages("arules",repos = "http://cran.us.r-project.org")

install.packages("arulesViz",repos = "http://cran.us.r-project.org")

library(arules)
library(arulesViz)

mymovies <- read.csv(file.choose())
View(mymovies)

rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))
rules

inspect(head(sort(rules, by = "lift"))) 

head(quality(rules))

plot(rules,method = "scatterplot")

plot(rules, method = "grouped")

plot(rules,method = "graph")
