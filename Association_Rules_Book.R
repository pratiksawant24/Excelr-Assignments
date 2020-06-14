##Association Rules Book

library(arules)
library(arulesViz)

data <- read.csv("C:\\PRATIK\\Data Science\\Assignment\\Association\\book.csv")
class(data)

colnames(data)

data$ChildBks <- factor(data$ChildBks,levels = c("1","0"),labels = c("ChildBks",""))
data$YouthBks <- factor(data$YouthBks,levels = c("1","0"),labels = c("YouthBks",""))
data$CookBks <- factor(data$CookBks,levels = c("1","0"),labels = c("CookBks",""))
data$DoItYBks <- factor(data$DoItYBks,levels = c("1","0"),labels = c("DoItYBks",""))
data$RefBks <- factor(data$RefBks,levels = c("1","0"),labels = c("RefBks",""))
data$ArtBks <- factor(data$ArtBks,levels = c("1","0"),labels = c("ArtBks",""))
data$GeogBks <- factor(data$GeogBks,levels = c("1","0"),labels = c("GeogBks",""))
data$ItalCook <- factor(data$ItalCook,levels = c("1","0"),labels = c("ItalCook",""))
data$ItalAtlas <- factor(data$ItalAtlas,levels = c("1","0"),labels = c("ItalAtlas",""))
data$ItalArt <- factor(data$ItalArt,levels = c("1","0"),labels = c("ItalArt",""))
data$Florence <- factor(data$Florence,levels = c("1","0"),labels = c("Florence",""))

data1 <- as(data,"transactions")

itemFrequencyPlot(data1,topN=25)

rules <- apriori(data1, parameter = list(supp = 0.005, confidence = 0.50, minlen = 2, maxlen = 4))

inspect(head(sort(rules), n = 10))

plot(head(sort(rules, by = "lift"), n = 10), method = "graph", control = list(cex = 1.0)) #cex = font size

plot(rules)

plot(head(sort(rules), n = 10), method = "grouped", control = list(cex = 0.2))