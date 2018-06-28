library(data.table)
library(MASS)    #Logistic
library(caret)   #Logistic

# Read the stats
shots <- fread("C:/Users/User/Desktop/資料探勘/Final Project/data_reduced_haha.csv", 
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
#####Logistic Regression#####
# Fit the model
fit <- glm(train$shot_made_flag ~ action_type + loc_x + loc_y + shot_zone_area +
             season + opponent + Guest_Home + season*playoffs, data = train, family = binomial(link = "logit"))
summary(fit)
fit$anova
capture.output(summary(fit),file="C:/Users/User/Desktop/資料探勘/Final Project/logistic_anova.doc")
# Stepwise
step <- stepAIC(fit, direction = "both")
step$anova

# Dividing the training dataset into 80/20. 
set.seed(1)
index<-createDataPartition(train$shot_made_flag, p= .8, list=FALSE)
cv_train <- train[index,]
cv_test <- train[-index,]

# CV
cv <- glm(cv_train$shot_made_flag ~ action_type + loc_x + loc_y + shot_zone_area +
            season + opponent + Guest_Home + season*playoffs , data = cv_train, family = binomial(link = "logit"))

cv_predict <- predict(cv, cv_test)
cv_exp_pred <- exp(cv_predict)
cv_odds <- cv_exp_pred/(1+cv_exp_pred)
cv_result <- ifelse(cv_odds > 0.5,1,0)

accuracy <- sum(cv_test$shot_made_flag==cv_result)/length(cv_result)
accuracy
# Prediction
pred <- predict(fit, test)
exp_pred <- exp(pred)
odds <- exp_pred/(1+exp_pred)
result <- ifelse(odds > 0.5,1,0)
output <- data.frame(result)
colnames(output) = c("shot_made_flag")
prediction_logistic <- odds

#####Prediction#####
output1 <- cbind(test$shot_id,prediction_logistic)
colnames(output1) <- c("shot_id","shot_made_flag")

write.table(output1, file = "C:/Users/User/Desktop/資料探勘/Final Project/output1.csv", sep = ",", col.names = NA )
