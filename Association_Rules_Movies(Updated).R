##Association Rule Movies

#install.packages("rmarkdown",repos = "http://cran.us.r-project.org")
#install.packages("arules",repos = "http://cran.us.r-project.org")
#install.packages("arulesViz",repos = "http://cran.us.r-project.org")

library(arules)
library(arulesViz)
library(rmarkdown)

mymovies <- read.csv(file.choose())

View(mymovies)

rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5))) # Provided the rules with 2% Support, 50 % Confidence and watched a minimum of 2 movies

rules

inspect(head(sort(rules, by = "lift"))) 

head(quality(rules))

plot(rules,method = "scatterplot")   #scatter plot for 77 Rules

plot(rules, method = "grouped")   #Grouped Matrix for 77 Rules

# It looks ike most of them has wateched Lord of the rings movies along with Gladiator and Greenville
# Also most of them has watched Gladiator, Sixth sense along with Patrioit
# Patriot ,Braveheart and other three items along with Gladiator.

plot(rules,method = "graph")   #Graph for 77 Rules
