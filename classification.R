cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
cat("++         FINAL PROJECT                                       ++\n")
cat("++         Dataset: Online Retail Dataset                      ++\n")
cat("++         Team Member: WENYU ZHANG, SIXUAN CHEN               ++\n")
cat("++         SUID: 233508014, 868010354                          ++\n")
cat("++         DATE: 5/08/2016                                     ++\n")
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
