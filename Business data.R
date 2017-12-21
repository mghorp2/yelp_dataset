#setwd("D:\UIC\Fall 2017\Advance stats\Project\Codes\Yelp Project")
library(jsonlite)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(FactoMineR)
library(factoextra)
# ***Business Data***


#head(df,3)
business<-stream_in(file("D:/UIC/Fall 2017/Advance stats/Project/yelp_dataset/business.json"))
head(business,3)
str(business)
business_flat<- flatten(business)
str(business_flat)
business_tbl<-as_data_frame(business_flat)
business_tbl

# Data exploration
categ<-business_tbl %>%mutate(categories=replace(categories,str_detect(categories, "Restaurant")|str_detect(categories, "Food"),"Restaurant"))
catego<-categ %>%unnest(categories)%>% dplyr::select(name,categories) %>% count(categories)%>% arrange(desc(n))
categories<- data.frame(catego)
categories$percent<-NA
categories$percent<- round((categories$n/sum(categories$n))*100,digit=2)
top_10_categories<-head(categories,10) 
new_row<-c("other Businesses",sum(categories$n)-sum(top_10_categories$n),100-sum(top_10_categories$percent))
top_cats<-rbind(top_10_categories,new_row)
x<-as.numeric(top_cats$percent)
pie(x,labels = paste(top_cats$categories,top_cats$percent,"%"),main="Pie Chart of Business")

# to check how many Restaurant categories are present
business_tbl %>% filter(str_detect(categories, "Restaurant")|str_detect(categories, "Food")) %>% mutate(categories = as.character(categories)) %>% dplyr::select(categories) # 65,057 rows contain "Restaurant" word in the variable categories  

# selecting only the rows which are related to Restaurant Business
Rest_Bus<-business_tbl %>% filter(str_detect(categories, "Restaurant")|str_detect(categories, "Food"))


#selecting states in USA
rest_bus<-Rest_Bus%>%filter(state %in% c("AZ","NV","OH","NC","PA","WI","IL","SC","NY","WA","CA"))

# To see the locations of these restaurants
states<-rest_bus%>%dplyr::select(state,categories)%>%count(state)%>%arrange(desc(n))
states<-data.frame(states)
states$percent<-NA
states$percent<- round((states$n/sum(states$n))*100,digit=2)
top_7_states<-head(states,7)
new_row_states<-c("Other States",sum(states$n)-sum(top_7_states$n),round(100-sum(top_7_states$percent),digits = 2))
top_states<-rbind(top_7_states,new_row_states)
x<-as.numeric(top_states$percent)
pie(x,labels = paste(top_states$state,x,"%"),col=rainbow(length(x)),main="Pie Chart of States in USA")

#count the restaurant ratings ratings
business_rating<-rest_bus%>%dplyr::select(name,stars)%>%count(stars)
barplot(business_rating$n,names.arg=c("1", "1.5", "2","2.5","3","3.5","4","4.5","5"), ylab="Count of Restaurants", xlab="Star Ratings", main="# of Restaurants per star",col = "Blue")



#removing variables from the business data
business_r<-rest_bus %>% dplyr::select(-categories,-latitude,-longitude,-starts_with("hours"))
#business_r[is.na(c(select(-starts_with("attributes"))))]<-"Unknown"
#head(business_r,2)
i = 1
col_names <- colnames(business_r)
for (i in ncol(business_r)){
  print(i)
  print(col_names[i])
  print(sum(is.na(business_r[,i])))
}

# removing columns which have more than 50% null values 

df_na<-data.frame(colnames=names(business_r),colSums_demo=colSums(is.na(business_r)))
typeof(df_na)
colnames(df_na)
#head(df_na,20)
df_na$percentage<-NA
df_na$percentage<-df_na$colSums_demo/nrow(business_r)
head(df_na,20)

per<-df_na%>%filter(percentage<=0.5)%>%dplyr::select(colnames)

business_r_filter<-business_r%>%dplyr::select(c(per$colnames))
#head(business_r_filter,5)
business_r_filter[is.na(business_r_filter)]<-"Unknown"
colSums(is.na(business_r_filter))

nrow(business_r_filter)
business_r_filter$business_stars<-NA
business_r_filter$business_stars<-business_r_filter$stars
business_r_filter<-business_r_filter%>%dplyr::select(-stars)
colSums(is.na(business_r_filter))


#selecting data only for illinois
business_final<-business_r_filter%>%filter(str_detect(state,"IL"))

#smp_size <- floor(0.07 * nrow(business_r_filter))
#set.seed(123)
#select_row <- sample(seq_len(nrow(business_r_filter)), size = smp_size)
#business_final<-business_r_filter[select_row,]




