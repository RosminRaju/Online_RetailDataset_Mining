# This is a test file for Rstudio Github project
# Counld be removed after testing
# If you make any modification to the .r file or the project
# Once you save the file 
# Commit and push into Github, it will be seen by other co-workers
install.packages("tm")
library(arules)
library(grid)
library(arulesViz)
library(tm)
raw_data <- read.csv("online_retail.csv")
raw_data$Item <- raw_data$Description
raw_data$Item <- as.character(raw_data$Item)
raw_data$Item <- sapply(raw_data$Item, FUN=function(x){strsplit(x, split = ' ')})
raw_data <- transform(raw_data, Item=unlist(Item))
raw_data$Item <- as.factor(raw_data$Item)
raw_data$Item <- tm_map(raw_data$Item, removeWords, stopwords("english")) 
#num_object<-length(raw_data[,9])
#for (i in 1:num_object){
#  x <- raw_data[i,9]
#  length_char <- lengths(raw_data[i,9],use.names = FALSE)
#  raw_data[i,9]<- x[[1]][[length_char]]
#}
#raw_data <- transform(raw_data, Item=unlist(Item))
#raw_data$Item <- as.factor(raw_data$Item)
arules_data <- raw_data[,c(1,9)]
write.csv(arules_data,file = "rules_data.csv")
transaction_data <- read.transactions("rules_data.csv", format = "single",sep = "\n",cols = c(1,3), rm.duplicate = TRUE)
arules <- apriori(transaction_data,parameter = list(minlen=2,supp=0.01,conf=0.2))
quality(arules) <- round(quality(arules), digits=3)
sorted_rules <- sort(arules,by="lift")
inspect(arules)
