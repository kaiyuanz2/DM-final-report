shots <- fread("C:/Users/User/Desktop/資料探勘/Final Project/data_reduced.csv", 
               data.table = F, stringsAsFactors = T)
train <- shots[!is.na(shots$shot_made_flag),]
test <- shots[is.na(shots$shot_made_flag),]

prediction_all <- cbind(prediction_logistic,prediction_xg)
colnames(prediction_all) <- c("logistic","xgboost")
a = 0
b = 1000
ensemble <- (prediction_logistic*a+prediction_xg*b)/(a+b)
output4 <- cbind(test$shot_id,ensemble)
colnames(output4) <- c("shot_id","shot_made_flag")

write.table(output4, file = "C:/Users/User/Desktop/資料探勘/Final Project/output4.csv", sep = ",", col.names = NA )

