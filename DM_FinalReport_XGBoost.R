install.packages("xgboost")
library(xgboost)
library(data.table)
library(Matrix)
completeData <- as.data.frame(fread("C:/Users/User/Desktop/資料探勘/Final Project/data.csv", header = T, stringsAsFactors = T))
train <- subset(completeData, !is.na(completeData$shot_made_flag));
test <- subset(completeData, is.na(completeData$shot_made_flag));
test.id <- test$shot_id;
train$shot_id <- NULL;
test$shot_id <- NULL;
train$time_remaining <- train$minutes_remaining*60+train$seconds_remaining;
test$time_remaining <- test$minutes_remaining*60+test$seconds_remaining;
train$shot_distance[train$shot_distance>45] <- 45;
test$shot_distance[test$shot_distance>45] <- 45;
train$seconds_remaining<-NULL;
test$seconds_remaining<-NULL;
train$team_name <- NULL;
test$team_name <- NULL;
train$team_id <- NULL;
test$team_id <- NULL;
train$game_event_id <- NULL;
test$game_event_id <- NULL;
train$game_id <- NULL;
test$game_id <- NULL;
train$lat <- NULL;
test$lat <- NULL;
train$lon <- NULL;
test$lon <- NULL;
train.y = train$shot_made_flag;
train$shot_made_flag <- NULL;
test$shot_made_flag <- NULL;
pred <- rep(0,nrow(test));
trainM<-data.matrix(train, rownames.force = NA);
dtrain <- xgb.DMatrix(data=trainM, label=train.y, missing = NaN);
watchlist <- list(trainM=dtrain);
param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "logloss",
                eta                 = 0.035,
                max_depth           = 4,
                subsample           = 0.40,
                colsample_bytree    = 0.40
)

clf <- xgb.cv(  params              = param, 
                data                = dtrain, 
                nrounds             = 1500, 
                verbose             = 1,
                watchlist           = watchlist,
                maximize            = FALSE,
                nfold               = 3,
                early.stop.round    = 10,
                print.every.n       = 1
);

best.nrounds = clf$best_iteration 

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = best.nrounds, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

testM <-data.matrix(test, rownames.force = NA);
Ypred = predict(clf, testM)
prediction_xg <- Ypred
Ypred <- cbind(test.id,Ypred)
colnames(Ypred) <- c("shot_id","shot_made_flag")
write.table(Ypred, file = "C:/Users/User/Desktop/資料探勘/Final Project/output_xg.csv", sep = ",", col.names = NA )
#########train ############
completeData <- as.data.frame(fread("C:/users/wade/Desktop/DM_report/data.csv", header = T, stringsAsFactors = T))
train <- subset(completeData, !is.na(completeData$shot_made_flag));
sample.data <- sample(nrow(train.data),round(0.8*nrow(train.data)))
train <- train[sample.data,]
test <- train[-sample.data,]
test.id <- test$shot_id;
train$shot_id <- NULL;
test$shot_id <- NULL;
train$time_remaining <- train$minutes_remaining*60+train$seconds_remaining;
test$time_remaining <- test$minutes_remaining*60+test$seconds_remaining;
train$shot_distance[train$shot_distance>45] <- 45;
test$shot_distance[test$shot_distance>45] <- 45;
train$seconds_remaining<-NULL;
test$seconds_remaining<-NULL;
train$team_name <- NULL;
test$team_name <- NULL;
train$team_id <- NULL;
test$team_id <- NULL;
train$game_event_id <- NULL;
test$game_event_id <- NULL;
train$game_id <- NULL;
test$game_id <- NULL;
train$lat <- NULL;
test$lat <- NULL;
train$lon <- NULL;
test$lon <- NULL;
train.y = train$shot_made_flag;
test.y = test$shot_made_flag;
train$shot_made_flag <- NULL;
test$shot_made_flag <- NULL;
pred <- rep(0,nrow(test));
trainM<-data.matrix(train, rownames.force = NA);
dtrain <- xgb.DMatrix(data=trainM, label=train.y, missing = NaN);
watchlist <- list(trainM=dtrain);
param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "logloss",
                eta                 = 0.035,
                max_depth           = 4,
                subsample           = 0.40,
                colsample_bytree    = 0.40
)

clf <- xgb.cv(  params              = param, 
                data                = dtrain, 
                nrounds             = 1500, 
                verbose             = 1,
                watchlist           = watchlist,
                maximize            = FALSE,
                nfold               = 3,
                early.stop.round    = 10,
                print.every.n       = 1
);

best.nrounds = clf$best_iteration 

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = best.nrounds, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

testM <-data.matrix(test, rownames.force = NA);
Ypred = predict(clf, testM)
Ypred <- ifelse(Ypred >= 0.5, "true", "false")
Ypred <- ifelse(Ypred == "true", 1, 0)
t0 = table(test.y,Ypred)
t0
sum(diag(t0))/sum(t0)
