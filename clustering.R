raw_data <- read.csv("online_retail.csv")
#install.packages("lubridate")
#library(lubridate)
#library(dplyr)
# Preprocess
# Remove instances without information about CustomerID
clean_data <- raw_data[!is.na(raw_data$CustomerID), ]
# Build Frequency-Amount model for clustering
cluster_data <- clean_data[,c(-2,-3,-8)]
cluster_data$Amount <- cluster_data$Quantity*cluster_data$UnitPrice
cluster_data$Date <- as.character(cluster_data$InvoiceDate)
cluster_data$Date <- sapply(cluster_data$Date, FUN=function(x){strsplit(x, split = ' ')[[1]][[1]]})
amount_data <- aggregate(cluster_data$Amount, by=list(cluster_data$CustomerID,cluster_data$InvoiceNo,cluster_data$Date), FUN=sum)
currentdate <- '2011-01-21'
amount_data$Date <- as.Date(amount_data$Group.3,format = '%m/%d/%y')
amount_data$Recency <- difftime(ymd(currentdate), ymd(amount_data$Date), unit="days")
amount_data$CustomerID <- amount_data$Group.1
amount_data$InvoiceNo <- amount_data$Group.2
amount_data$Amount <- amount_data$x
amount_data <- amount_data[,c(-1,-2,-3,-4,-5)]
#amount_data <- amount_data[1:2001,]       # Remove return transactions
amount_data$Frequency <- amount_data$Amount
num_object <- nrow(amount_data)
for(i in 1:num_object){
  amount_data[i,5] <- 1
}
Recency_data <- aggregate(cbind(amount_data$Recency), by=list(amount_data$CustomerID),FUN=min)
FM_data <- aggregate(cbind(amount_data$Amount,amount_data$Frequency), by=list(amount_data$CustomerID),FUN=sum)
RFM_data <- FM_data[order(FM_data[,1]),]
#sorted_amount <- amount_data[order(amount_data[,2]),]
RFM_data$Rec <- Recency_data$V1
RFM_data$CustomerID <- RFM_data$Group.1
RFM_data$Recency <- RFM_data$Rec
RFM_data$Frequency <- RFM_data$V2
RFM_data$Amount <- RFM_data$V1
RFM_data <- RFM_data[,c(-1,-2,-3,-4)]
write.csv(RFM_data,"RFM_dataset.csv")
