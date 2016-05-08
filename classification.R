cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
cat("++         FINAL PROJECT                                       ++\n")
cat("++         Dataset: Online Retail Dataset                      ++\n")
cat("++         Team Member: WENYU ZHANG, SIXUAN CHEN               ++\n")
cat("++         SUID: 233508014                                     ++\n")
cat("++         DATE: 4/25/2016                                     ++\n")
cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
library("RWeka")
library("arules")
# Classification using RFM dataset
classification_data <- read.csv("RFM_dataset.csv")
classification_data <- classification_data[,-1]
str(classification_data)
# Step 1: Discretize class attribute "Cluster"
classification_data$Cluster <- discretize(classification_data$Cluster, method = "interval",categories=4,labels = c("1","2","3","4"))

# Step 2: Build trainning dataset and test dataset
train_data <- classification_data[1:802,]
train_data <- train_data[,-1]
test_data <- classification_data[803:1204,]
test_data <- test_data[,-1]

# Step 3: Build decision tree using trainning data
decision_tree1 <- J48(Cluster ~ ., data = train_data, control = Weka_control( M = 5, C = 0.1, B = F))

# Step 4: Test decision tree
# Use 10-folds cross validation
evaluate_Weka_classifier(decision_tree1, class = T, numFolds = 10)
# Use new test dataset
evaluate_Weka_classifier(decision_tree1, newdata = test_data, class = T)
# Both decision tree models have high accuracies


# Classification using raw dataset
raw_data <- read.csv("raw_data.csv")
raw_data <- raw_data[,-1]
raw_data$Cluster <- 0
num_raw_data <- length(raw_data[,1])
num_classification_data <- length(classification_data[,1])
str(raw_data)
for(i in 1:num_raw_data){
  for(j in 1:num_classification_data){
    if(raw_data[i,7]==classification_data[j,1]){
      raw_data[i,9] <- classification_data[j,5]
      next
    }
  }
}
write.csv(raw_data,file="raw_data_classification.csv")
