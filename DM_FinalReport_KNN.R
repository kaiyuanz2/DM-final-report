library(data.table)
library(class)   #KNN
library(dplyr)   #KNN
# Read the stats
shots <- fread("C:/Users/User/Desktop/��Ʊ���/Final Project/data_reduced.csv", 
               data.table = F, stringsAsFactors = T)
shots$time_remaining <- shots$minutes_remaining * 60 + shots$seconds_remaining
# View first 6 rows
head(shots)

# Structure of data set
str(shots)

train <- shots[!is.na(shots$shot_made_flag),]
test <- shots[is.na(shots$shot_made_flag),]
nrow(train)
train$shot_made_flag <- as.factor(train$shot_made_flag)
train$period <- as.factor(train$period)
train$playoffs <- as.factor(train$playoffs)

test$shot_made_flag <- NULL
test$period <- as.factor(test$period)
test$playoffs <- as.factor(test$playoffs)
head(train)
head(test)
#####KNN#####
str(train)
train.data.knn <- train[,c(3,4,5,6,12,13,21)] 
test.data.knn <- test[,c(3,4,5,6,12,20)]
head(train.data.knn)
head(test.data.knn)
#���Xshot_made_flag
trainLabels <- train[, 13] 
#�R��knnTrain��knnTest����shot_made_flag
knnTrain <- train.data.knn[,-6]
knnTest <- test.data.knn

head(knnTrain)
head(knnTest)
#�p��k��(�X�ӾF�~)�q�`�i�H�θ�Ƽƪ������
kv <- round(sqrt(nrow(knnTrain)))

#(4)�إ߼ҫ� 
prediction_knn <- knn(train = knnTrain, test = knnTest, cl = trainLabels, k = kv)
prediction_knn <- as.numeric(prediction_knn)-1


#####Prediction#####
output2 <- cbind(test$shot_id,prediction_knn)
colnames(output2) <- c("shot_id","shot_made_flag")

write.table(output2, file = "C:/Users/User/Desktop/��Ʊ���/Final Project/output2.csv", sep = ",", col.names = NA )