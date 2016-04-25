cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
cat("++         FINAL PROJECT                                       ++\n")
cat("++         Dataset: Online Retail Dataset                      ++\n")
cat("++         Team Member: WENYU ZHANG, SIXUAN CHEN               ++\n")
cat("++         SUID: 233508014                                     ++\n")
cat("++         DATE: 4/08/2016                                     ++\n")
cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
#install.packages("lubridate")
#install.packages("outliers")
library(lubridate)
library(dplyr)
library(outliers)
library(ggplot2)
library(reshape2)
raw_data <- read.csv("online_retail.csv")

# Step 1: Remove instances without information about CustomerID
clean_data <- raw_data[!is.na(raw_data$CustomerID), ]

# Step 2: Build Frequency-Amount model for clustering
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
amount_data$Frequency <- amount_data$Amount
num_object <- nrow(amount_data)
for(i in 1:num_object){
  amount_data[i,5] <- 1
}
FM_data <- aggregate(cbind(amount_data$Amount,amount_data$Frequency), by=list(amount_data$CustomerID),FUN=sum)
FM_data$CustomerID <- FM_data$Group.1
FM_data$Amount <- FM_data$V1
FM_data$Frequency <- FM_data$V2
FM_data <- FM_data[,c(-1,-2,-3)]
# Deal with the instances with amount<0 in FM_data 
# Simplely assign 0 to those values
for(i in 1:1204){
  if(FM_data[i,2]<0){
    FM_data[i,2] <- 0
  }
}

# Step 3: Build Recency-Frequency-Amount model for clustering
Recency_data <- aggregate(cbind(amount_data$Recency), by=list(amount_data$CustomerID),FUN=min)
RFM_data <- FM_data[order(FM_data[,1]),]
RFM_data$Recency <- Recency_data$V1

# Step 4: Deal with outliers
x <- RFM_data$Amount
y <- RFM_data$Frequency
outlier.IQR_amount <- function(x, multiple = 1.5, replace = TRUE, revalue = 1147) { 
  q <- quantile(x) 
  IQR <- q[4] - q[2]
  x1 <- which(x < q[2] - multiple * IQR | x > q[4] + multiple * IQR)
  x2 <- x[x1]
  if (length(x2) > 0) outlier <- data.frame(location = x1, value = x2)
  else outlier <- data.frame(location = 0, value = 0)
  if (replace == TRUE) {
    x[x1] <- revalue
  }
  return(list(new.value = x, outlier = outlier))
}
outlier.IQR_freq <- function(x, multiple = 1.5, replace = TRUE, revalue = 4) { 
  q <- quantile(x) 
  IQR <- q[4] - q[2]
  x1 <- which(x < q[2] - multiple * IQR | x > q[4] + multiple * IQR)
  x2 <- x[x1]
  if (length(x2) > 0) outlier <- data.frame(location = x1, value = x2)
  else outlier <- data.frame(location = 0, value = 0)
  if (replace == TRUE) {
    x[x1] <- revalue
  }
  return(list(new.value = x, outlier = outlier))
}
replace_amount <- outlier.IQR_amount(x)$new.value
replace_freq <- outlier.IQR_freq(y)$new.value
RFM_data$Amount <- replace_amount
RFM_data$Frequency <- replace_freq
write.csv(RFM_data,file="RFM_dataset.csv")

# Normalize the RFM dataset with min-max normalization
# And assume the range of 3 attributes after normalizing is from 0 to 100
RFM_data_normalized <- RFM_data
max_amount <- max(RFM_data_normalized$Amount)
min_amount <- min(RFM_data_normalized$Amount)
range_amount <- max_amount-min_amount
max_frequency <- max(RFM_data_normalized$Frequency)
min_frequency <- min(RFM_data_normalized$Frequency)
range_frequency <- max_frequency-min_frequency
max_recency <- max(RFM_data_normalized$Recency)
min_recency <- min(RFM_data_normalized$Recency)
range_recency <- max_recency-min_recency
for(i in 1:1204){
  RFM_data_normalized[i,2] <- 100*(RFM_data_normalized[i,2]-min_amount)/range_amount
  RFM_data_normalized[i,3] <- 100*(RFM_data_normalized[i,3]-min_frequency)/range_frequency
  RFM_data_normalized[i,4] <- 100*(max_recency-RFM_data_normalized[i,4])/range_recency
}
write.csv(RFM_data_normalized,file="RFM_normalized.csv")

# Step 5: Choose the best number of clusters
# Utilize visualization method, plot the figure with variable number of clusters, k, against SSE, the SSE values are calculated by Weka
k=c(3,4,5,6,7,8,9)
sse=c(118.3892,95.8689,80.1267,70.1901,58.9925,54.1611,48.9482)
SSE <- melt(k)
SSE$k <- k
SSE$SSE_value <- sse
SSE <- SSE[,-1]
ggplot(SSE, aes(x=k, y=SSE_value)) + geom_line() + geom_point(size=4, shape=20)
# From the plot, we can tell that k=4 is the best number for clustering

# Step 6: Cluster the dataset using k-means where k=4
# For RFM dataset
RFM_cluster <- RFM_data_normalized
RFM_cluster$CustomerID <- NULL
RFM_kmeans <- kmeans(RFM_cluster,4)
(RFM_kmeans)
