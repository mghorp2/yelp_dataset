library(xgboost)
library(dplyr)
library(caret)
yelp_data = read.csv("D:/UIC/Fall 2017/Advance stats/Project/Codes/Yelp Project/yelp_data_xgb.csv")
review_xgb_test=read.csv("D:/UIC/Fall 2017/Advance stats/Project/Codes/Yelp Project/yelp_data_xgb_test.csv")

head(yelp_data,5)
colnames(yelp_data)

#filtering the data and selecting the same variables given by random forest
yelp_data_filtered = yelp_data[,c(2,3,6,12,13,14,15,17,18,19,20,21,22,23,24,25,27,28,29,31,32,33,34,35,36,37,38,39,40,43,45,47,50,54,55,57)]
head(yelp_data_filtered,5)
str(yelp_data_filtered)
summary(yelp_data_filtered)
review_tr<-yelp_data_filtered
review_tr$record_number<-NA
review_tr$record_number<-rownames(review_tr)
review_train = yelp_data_filtered
review_train[sapply(review_train, is.character)] <- lapply(review_train[sapply(review_train, is.character)],  as.factor)
review_train[sapply(review_train, is.factor)] <- lapply(review_train[sapply(review_train, is.factor)],  as.numeric)
review_train$stars <- as.factor(review_train$stars)
review_train$business_stars <- as.factor(review_train$business_stars)
colnames(review_train)[which(names(review_train) == "attributes.DietaryRestrictions.gluten-free")] <- "attributes.DietaryRestrictions.glutenfree"
colnames(review_train)[which(names(review_train) == "attributes.DietaryRestrictions.soy-free")] <- "attributes.DietaryRestrictions.soyfree"
colnames(review_train)[which(names(review_train) == "attributes.DietaryRestrictions.dairy-free")] <- "attributes.DietaryRestrictions.dairyfree"

str(review_train)

unique(review_train$attributes.GoodForMeal.dessert)
review_train = review_train%>% dplyr::select(-business_id)
train_label = review_train$business_stars
review_train = review_train%>% dplyr::select(-business_stars)
nunmber_classes  = length(unique(train_label))

train_data <- xgb.DMatrix(data.matrix(review_train),label = train_label)
bstSparse <- xgboost(train_data,max_depth = 6, eta = 1,nrounds = 200,verbose  = 1,print_every_n = 50,'objective' = 'multi:softmax',num_class = nunmber_classes+1)
bstSparse_prob <- xgboost(train_data,max_depth = 6, eta = 1,nrounds = 200,verbose  = 1,print_every_n = 50,'objective' = 'multi:softprob',num_class = nunmber_classes+1)
summary(bstSparse)
summary(bstSparse_prob)
predict_xgb <- predict(bstSparse,data.matrix(review_train))
gbm_train_pred<-predict_xgb

predict_xgb_prob <- predict(bstSparse_prob,data.matrix(review_train),type = "prob")
predict_xgb_prob

score_max_train_gbm<-predict_xgb_prob
predict_output_gbm<-cbind.data.frame(gbm_train_pred,score_max_train_gbm)
predict_output_gbm$record_number<-rownames(predict_output_gbm)
head(predict_output_gbm,3)

Review_train_data_gbm<-merge(x=review_tr,y=predict_output_gbm,by="record_number",all.x= TRUE)
head(Review_train_data_gbm,2)
Review_train_data_gbm<-Review_train_data_gbm%>%dplyr::select(record_number,business_id,business_stars,gbm_train_pred,score_max_train_gbm)%>%arrange(business_id,desc(score_max_train_gbm))
head(Review_train_data_gbm,20)

rf_train_pred_gbm<-Review_train_data_gbm[!duplicated(Review_train_data_gbm$business_id), ]  
head(rf_train_pred_gbm,20)

rf_train_pred_gbm$try<-NA
rf_train_pred_gbm$try[rf_train_pred_gbm$gbm_train_pred==1]<-1
rf_train_pred_gbm$try[rf_train_pred_gbm$gbm_train_pred==2]<-1.5
rf_train_pred_gbm$try[rf_train_pred_gbm$gbm_train_pred==3]<-2
rf_train_pred_gbm$try[rf_train_pred_gbm$gbm_train_pred==4]<-2.5
rf_train_pred_gbm$try[rf_train_pred_gbm$gbm_train_pred==5]<-3
rf_train_pred_gbm$try[rf_train_pred_gbm$gbm_train_pred==6]<-3.5
rf_train_pred_gbm$try[rf_train_pred_gbm$gbm_train_pred==7]<-4
rf_train_pred_gbm$try[rf_train_pred_gbm$gbm_train_pred==8]<-4.5
rf_train_pred_gbm$try[rf_train_pred_gbm$gbm_train_pred==9]<-5

# performance of GBM on training data
cm_gbm_train <- confusionMatrix(rf_train_pred_gbm$try,rf_train_pred_gbm$business_stars)
cm_gbm_train




# test 

#filtering the data and selecting the same variables given by random forest
yelp_data_test = review_xgb_test
yelp_data_filtered_test = yelp_data_test[,c(2,3,6,12,13,14,15,17,18,19,20,21,22,23,24,25,27,28,29,31,32,33,34,35,36,37,38,39,40,43,45,47,50,54,55,57)]
head(yelp_data_filtered_test,5)

head(yelp_data_filtered_test,5)
str(yelp_data_filtered_test)
summary(yelp_data_filtered_test)
review_te_xgb<-review_xgb_test
review_te_xgb$record_number<-NA
review_te_xgb$record_number<-rownames(review_te_xgb)
str(review_te_xgb)
review_test_xgb = yelp_data_filtered_test
review_test_xgb[sapply(review_test_xgb, is.character)] <- lapply(review_test_xgb[sapply(review_test_xgb, is.character)],  as.factor)
review_test_xgb[sapply(review_test_xgb, is.factor)] <- lapply(review_test_xgb[sapply(review_test_xgb, is.factor)],  as.numeric)
review_test_xgb$stars <- as.factor(review_test_xgb$stars)
review_test_xgb$business_stars <- as.factor(review_test_xgb$business_stars)
colnames(review_test_xgb)[which(names(review_test_xgb) == "attributes.DietaryRestrictions.gluten-free")] <- "attributes.DietaryRestrictions.glutenfree"
colnames(review_test_xgb)[which(names(review_test_xgb) == "attributes.DietaryRestrictions.soy-free")] <- "attributes.DietaryRestrictions.soyfree"
colnames(review_test_xgb)[which(names(review_test_xgb) == "attributes.DietaryRestrictions.dairy-free")] <- "attributes.DietaryRestrictions.dairyfree"

str(review_test_xgb)

unique(review_test_xgb$attributes.GoodForMeal.dessert)
review_test_xgb = review_test_xgb%>% dplyr::select(-business_id)
train_label_test = review_test_xgb$business_stars
review_test_xgb = review_test_xgb%>% dplyr::select(-business_stars)
nunmber_classes  = length(unique(train_label_test))

predict_xgb_test <- predict(bstSparse,data.matrix(review_test_xgb))
gbm_test_pred<-predict_xgb_test

predict_xgb_prob_test <- predict(bstSparse_prob,data.matrix(review_test_xgb),type = "prob")
predict_xgb_prob_test

score_max_test_gbm<-predict_xgb_prob_test
predict_output_gbm_test<-cbind.data.frame(gbm_test_pred,score_max_test_gbm)
predict_output_gbm_test$record_number<-rownames(predict_output_gbm_test)
head(predict_output_gbm_test,3)

Review_test_data_gbm<-merge(x=review_te_xgb,y=predict_output_gbm_test,by="record_number",all.x= TRUE)
head(Review_test_data_gbm,2)
Review_test_data_gbm<-Review_test_data_gbm %>% dplyr::select(record_number,business_id,business_stars,gbm_test_pred,score_max_test_gbm) %>% dplyr::arrange(business_id,desc(score_max_test_gbm))
head(Review_test_data_gbm,20)

rf_test_pred_gbm<-Review_test_data_gbm[!duplicated(Review_test_data_gbm$business_id), ]  
head(rf_test_pred_gbm,20)

rf_test_pred_gbm$try<-NA
rf_test_pred_gbm$try[rf_test_pred_gbm$gbm_test_pred==1]<-1
rf_test_pred_gbm$try[rf_test_pred_gbm$gbm_test_pred==2]<-1.5
rf_test_pred_gbm$try[rf_test_pred_gbm$gbm_test_pred==3]<-2
rf_test_pred_gbm$try[rf_test_pred_gbm$gbm_test_pred==4]<-2.5
rf_test_pred_gbm$try[rf_test_pred_gbm$gbm_test_pred==5]<-3
rf_test_pred_gbm$try[rf_test_pred_gbm$gbm_test_pred==6]<-3.5
rf_test_pred_gbm$try[rf_test_pred_gbm$gbm_test_pred==7]<-4
rf_test_pred_gbm$try[rf_test_pred_gbm$gbm_test_pred==8]<-4.5
rf_test_pred_gbm$try[rf_test_pred_gbm$gbm_test_pred==9]<-5

# performance of GBM on training data
cm_gbm_test <- confusionMatrix(rf_test_pred_gbm$try,rf_test_pred_gbm$business_stars)
cm_gbm_test
