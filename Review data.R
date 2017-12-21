setwd("D:/UIC/Fall 2017/Advance stats/Project/Codes/Yelp Project")

library(jsonlite)
library(RJSONIO)
library(tibble)
library(tidyr)
library(stringr)
library(RColorBrewer)
library(NLP)
library(tm)
library(syuzhet)
library(SnowballC)
library(tmap)
library(wordcloud)
library(tidytext)
library(randomForest)
library(caret)
library(MASS)
library(nnet)
library(e1071)
library(xgboost)
library(dplyr)


# ***Business Data***

review_t <- stream_in(file("D:/UIC/Fall 2017/Advance stats/Project/yelp_dataset/review.json"))
head(review_t,3)
str(review_t)
review_flat<- flatten(review_t)
str(review_flat)
reviews_tbl<-as_data_frame(review_flat)



#merging with business
head(business_final,2)
Bus_review<-merge(x=business_final,y=reviews_tbl,by="business_id",all.x= TRUE)
head(Bus_review,2)
str(Bus_review)


tot<-Bus_review %>% dplyr::select(stars) %>% count(stars)
barplot(tot$n,names.arg=c("1", "2","3","4","5"), ylab="Count of Reviews", xlab="Star Ratings",main="Number of Reviews per Review star",col="Blue")


# cheching the frequency of each word in the data


# for business stars with 5-4
print(Sys.time())
review_text_3<-Bus_review%>%filter(business_stars %in% c("5","4.5","4"))%>%dplyr::select(text)
df = data.frame()
df.new = data.frame()
for (i in 1:1000){
  var = review_text_3[i,]
  q3 <- Corpus(VectorSource(var))
  q3 <- tm_map(q3, removePunctuation)  
  q3 <- tm_map(q3, tolower)   
  # q1 <- tm_map(q1, stemDocument,language = "english")   
  # to remove white space stripWhitespace
  q3 <- tm_map(q3, stripWhitespace)   
  q3 <- tm_map(q3, removeWords, stopwords("english"))   
  
  # to convert documents into text documents...this tells R to treat preprocessed document as text documents PlainTextDocument
  q3 <- tm_map(q3, PlainTextDocument)
  doc = TermDocumentMatrix(q3,control = list(minWordLength = 2)) 
  a11 = doc$dimnames$Terms 
  #print(a11)
  df.new = cbind(a11)
  df = rbind(df,df.new)
  
  if (i %% 1000 ==0 ){
    print("Processing Record Number:")
    print(i)
    #print(a11)
  }
}

df=as.matrix(df)
wordcloud(df,scale = c(3, 0.5), max.words = 50, min.freq = 100, random.order = , 
          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))
 

# for business stars with 1,1.5
review_text_1<-Bus_review%>%filter(business_stars %in% c("1.5","1"))%>%dplyr::select(text)
df_1 = data.frame()
df.new_1 = data.frame()
for (i in 1:5000){
  var = review_text_1[i,]
  q1 <- Corpus(VectorSource(var))
  q1 <- tm_map(q1, removePunctuation)  
  q1 <- tm_map(q1, tolower)   
  # q1 <- tm_map(q1, stemDocument,language = "english")   
  # to remove white space stripWhitespace
  q1 <- tm_map(q1, stripWhitespace)   
  q1 <- tm_map(q1, removeWords, stopwords("english"))   
  
  # to convert documents into text documents...this tells R to treat preprocessed document as text documents PlainTextDocument
  q1 <- tm_map(q1, PlainTextDocument)
  doc = TermDocumentMatrix(q1,control = list(minWordLength = 2)) 
  a11 = doc$dimnames$Terms 
  #print(a11)
  df.new_1 = cbind(a11)
  df_1 = rbind(df_1,df.new_1)
  
  if (i %% 1000 ==0 ){
    print("Processing Record Number:")
    print(i)
    #print(a11)
  }
  
}

df_1=as.matrix(df_1)
wordcloud(df_1,scale = c(3, 0.5), max.words = 50, min.freq = 100, random.order = , 
          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))



# Performing sentimental Analysis on whole data "text" column

df_train = data.frame()
df.new_train = data.frame()
for (i in 1:nrow(Bus_review)){

  #getting positive and negative sentiments

  sentiment_1<-get_nrc_sentiment(Bus_review$text[i])
 # print(sentiment_train)
  pos<-sum(sentiment_1$positive+sentiment_1$joy+sentiment_1$trust)
  neg<-sum(sentiment_1$negative+sentiment_1$anger+sentiment_1$disgust+sentiment_1$sadness) 
  
  df.new_train = cbind(pos,neg)
  df_train = rbind(df_train,df.new_train)
  
  if (i %% 1000==0 ){
    print("Processing Record Number:")
    print(i)
    #print(a11)
  }
  
}


#adding the positive sentiments and negative sentiments in the data
review_senti<-cbind(Bus_review,df_train)
head(review_senti,2)
write.csv(review_senti,file="review_business.csv")

#spliting the data into training and test

smp_size_train <- floor(0.6 * nrow(review_senti))
set.seed(123)
select_row_train <- sample(seq_len(nrow(review_senti)), size = smp_size_train)
review_train<-review_senti[select_row_train,]
review_test<-review_senti[-select_row_train,]
write.csv(review_train,"yelp_data_xgb.csv")
write.csv(review_test,"yelp_data_xgb_test.csv")
review_xgb_train<-review_train
review_xgb_test<-review_test
review_tr<-review_senti[select_row_train,]
review_te<-review_senti[-select_row_train,]


# selecting specific columns from training data

str(review_train)
review_train<-review_train %>% dplyr::select(-business_id,-address,-state,-postal_code,-city,-review_id,-user_id,-date,-text)
str(review_train)
review_train[sapply(review_train, is.character)] <- lapply(review_train[sapply(review_train, is.character)],  as.factor)
review_train[sapply(review_train, is.factor)] <- lapply(review_train[sapply(review_train, is.factor)],  as.numeric)
review_train$stars <- as.factor(review_train$stars)
review_train$business_stars <- as.factor(review_train$business_stars)
colnames(review_train)[which(names(review_train) == "attributes.DietaryRestrictions.gluten-free")] <- "attributes.DietaryRestrictions.glutenfree"
colnames(review_train)[which(names(review_train) == "attributes.DietaryRestrictions.soy-free")] <- "attributes.DietaryRestrictions.soyfree"
colnames(review_train)[which(names(review_train) == "attributes.DietaryRestrictions.dairy-free")] <- "attributes.DietaryRestrictions.dairyfree"

str(review_train)

## ****Performing Random Forest classifier ****

# variable selection using random forest
set.seed(423)
rf_tarin_1 <- randomForest(business_stars ~ . , data=review_train, importance=TRUE, ntree=500)

imp<-importance(rf_tarin_1)
vars<-dimnames(imp)[[1]]
imp<-data.frame(vars=vars,imp=as.numeric(imp[,1]))
imp<-imp[order(imp$imp,decreasing=T),]

selected<-c(as.character(imp[1:35,1]),'business_stars')

# performing Random forest
set.seed(423)
rf_tarin <- randomForest(business_stars ~ . , data=review_train[,selected], importance=TRUE, ntree=500)

varImpPlot(rf_tarin,main='Variable Importance',pch=16,col='blue')



#prediction on training data
predict_train<-predict(rf_tarin,data=review_train,type="class")
score_train=predict(rf_tarin,review_train, type="prob")
score_max_train<-apply(score_train,1,max)
head(score_max_train,2)
predict_output<-cbind.data.frame(predict_train,score_max_train)
predict_output$record_number<-rownames(predict_output)
head(predict_output,3)

review_tr$record_number<-rownames(review_tr)
head(review_tr,3)

Review_train_data<-merge(x=review_tr,y=predict_output,by="record_number",all.x= TRUE)
head(Review_train_data,2)
Review_train_data<-Review_train_data%>% dplyr::select(record_number,business_id,business_stars,predict_train,score_max_train)%>%arrange(business_id,desc(score_max_train))
head(Review_train_data,20)

rf_train_pred<-Review_train_data[!duplicated(Review_train_data$business_id), ]  
head(rf_train_pred,20)


# performance of random forest on training data
cm_rf_train <- confusionMatrix(rf_train_pred$predict_train,rf_train_pred$business_stars)
cm_rf_train


#performing selection of columns on test data

str(review_test)
review_test<-review_test%>%dplyr::select(-business_id,-address,-state,-postal_code,-city,-review_id,-user_id,-date,-text)
str(review_test)
review_test[sapply(review_test, is.character)] <- lapply(review_test[sapply(review_test, is.character)],  as.factor)
review_test[sapply(review_test, is.factor)] <- lapply(review_test[sapply(review_test, is.factor)],  as.numeric)
review_test$stars <- as.factor(review_test$stars)
review_test$business_stars <- as.factor(review_test$business_stars)
colnames(review_test)[which(names(review_test) == "attributes.DietaryRestrictions.gluten-free")] <- "attributes.DietaryRestrictions.glutenfree"
colnames(review_test)[which(names(review_test) == "attributes.DietaryRestrictions.soy-free")] <- "attributes.DietaryRestrictions.soyfree"
colnames(review_test)[which(names(review_test) == "attributes.DietaryRestrictions.dairy-free")] <- "attributes.DietaryRestrictions.dairyfree"

str(review_test)

#prediction on test data
predict_rf_test<-predict(rf_tarin,review_test[,selected],type="class")
score_test=predict(rf_tarin,review_test, type="prob")
score_max_test<-apply(score_test,1,max)
head(score_max_test,2)
predict_output_test<-cbind.data.frame(predict_rf_test,score_max_test)
predict_output_test$record_number<-rownames(predict_output_test)
head(predict_output_test,3)

review_te$record_number<-rownames(review_te)
head(review_te,3)

Review_test_rf<-merge(x=review_te,y=predict_output_test,by="record_number",all.x= TRUE)
head(Review_test_rf,2)
Review_test_rf<-Review_test_rf%>% dplyr::select(record_number,business_id,business_stars,predict_rf_test,score_max_test)%>%arrange(business_id,desc(score_max_test))
head(Review_test_rf,20)

rf_test_pred<-Review_test_rf[!duplicated(Review_test_rf$business_id), ]  
head(rf_test_pred,20)

# performance of random forest on test data
cm_rf_test <- confusionMatrix(rf_test_pred$predict_rf_test,rf_test_pred$business_stars)
cm_rf_test

# **** Performing SVM ***


train_svm<-svm(business_stars~.,data=review_train,probability=TRUE)
summary(train_svm)


# prediction on training data
svm_train_pred<-predict(train_svm,data=review_train,type="class")
SVM_score_train=predict(train_svm,review_train, probability=TRUE)
SVM_train_prob<-attr(SVM_score_train, "probabilities")
score_max_train_svm<-apply(SVM_train_prob,1,max)
predict_output_svm<-cbind.data.frame(svm_train_pred,score_max_train_svm)
predict_output_svm$record_number<-rownames(predict_output_svm)
head(predict_output_svm,3)

Review_train_data_svm<-merge(x=review_tr,y=predict_output_svm,by="record_number",all.x= TRUE)
head(Review_train_data_svm,2)
Review_train_data_svm<-Review_train_data_svm%>%dplyr::select(record_number,business_id,business_stars,svm_train_pred,score_max_train_svm)%>%arrange(business_id,desc(score_max_train_svm))
head(Review_train_data_svm,20)

rf_train_pred_svm<-Review_train_data_svm[!duplicated(Review_train_data_svm$business_id), ]  
head(rf_train_pred_svm,20)


# performance of SVM on training data
cm_svm_train <- confusionMatrix(rf_train_pred_svm$svm_train_pred,rf_train_pred_svm$business_stars)
cm_svm_train


# prediction on test data
svm_test_pred<-predict(train_svm,review_test,type="class")
SVM_score_test=predict(train_svm,review_test, probability=TRUE)
SVM_test_prob<-attr(SVM_score_test, "probabilities")
score_max_test_svm<-apply(SVM_test_prob,1,max)
predict_output_svm_t<-cbind.data.frame(svm_test_pred,score_max_test_svm)
predict_output_svm_t$record_number<-rownames(predict_output_svm_t)
head(predict_output_svm_t,3)

Review_test_svm<-merge(x=review_te,y=predict_output_svm_t,by="record_number",all.x= TRUE)
head(Review_test_svm,2)
Review_test_svm<-Review_test_svm%>%dplyr::select(record_number,business_id,business_stars,svm_test_pred,score_max_test_svm)%>%arrange(business_id,desc(score_max_test_svm))
head(Review_test_svm,20)

svm_test_pred<-Review_test_svm[!duplicated(Review_test_svm$business_id), ]  
head(svm_test_pred,20)

# performance of SVM on test data
cm_svm_test <- confusionMatrix(svm_test_pred$svm_test_pred,svm_test_pred$business_stars)
cm_svm_test

#naive bayes


a<-review_train[,selected]
x<-a%>% dplyr::select(-business_stars)
head(x,2)
model = train(x,review_train$business_stars,'nb',trControl=trainControl(method='cv',number=10))
#nb_train_model<-naiveBayes(business_stars ~ ., data = review_train )
model

pred<-predict(model$finalModel,newdata=review_train)
nb_train_pred = pred$class
nb_score_train=pred$posterior
score_max_train_nb<-apply(nb_score_train,1,max)
predict_output_nb<-cbind.data.frame(nb_train_pred,score_max_train_nb)
predict_output_nb$record_number<-rownames(predict_output_nb)
head(predict_output_nb,3)

Review_train_data_nb<-merge(x=review_tr,y=predict_output_nb,by="record_number",all.x= TRUE)
head(Review_train_data_nb,2)
Review_train_data_nb<-Review_train_data_nb%>% select(record_number,business_id,business_stars,gbm_train_pred,score_max_train_nb)%>%arrange(business_id,desc(score_max_train_nb))
head(Review_train_data_nb,20)

rf_train_pred_nb<-Review_train_data_nb[!duplicated(Review_train_data_nb$business_id), ]  
head(rf_train_pred_nb,20)

# performance of naive bayes on training data
cm_nb_train <- confusionMatrix(rf_train_pred_nb$nb_train_pred,rf_train_pred_nb$business_stars)
cm_nb_train

x1<-review_test[,selected]%>%dplyr::select(-business_stars)
# prediction on test data
test_pred<-predict(model$finalModel,newdata=review_test)
nb_test_pred<-test_pred$class
nb_score_test=test_pred$posterior
score_max_test_nb <- apply(nb_score_test,1,max)
predict_output_nb_t <- cbind.data.frame(nb_test_pred,score_max_test_nb)
predict_output_nb_t$record_number<-rownames(predict_output_nb_t)
head(predict_output_nb_t,30)

Review_test_nb<-merge(x=review_te,y=predict_output_nb_t,by="record_number",all.x= TRUE)
head(Review_test_nb,2)
Review_test_nb<-Review_test_nb%>% select(record_number,business_id,business_stars,nb_test_pred,score_max_test_nb)%>%arrange(business_id,desc(score_max_test_nb))
head(Review_test_nb,20)

nb_test_pred<-Review_test_nb[!duplicated(Review_test_nb$business_id), ]  
head(nb_test_pred,20)

# performance of SVM on test data
cm_nb_test <- confusionMatrix(nb_test_pred$nb_test_pred,nb_test_pred$business_stars)
cm_nb_test


