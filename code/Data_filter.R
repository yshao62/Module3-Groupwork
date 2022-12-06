library(rjson)
library(jsonlite)
setwd("C:/Users/adin/Desktop/stat628/Module3/git")
business <- jsonlite::stream_in(file("../../business.json"),pagesize = 100)
review=jsonlite::stream_in(file("data/review.json"),pagesize = 10000)
review_information=review[c("user_id","stars")]
write.csv(review_information,"review_stars.csv",row.names = FALSE)
user=jsonlite::stream_in(file("../data/user.json"),pagesize = 10000)

#find important user
biguser=user[user["fans"]>0,c(1,2,3,5,6,7,10,11)]
write.csv(biguser,"important_user0.csv",row.names = FALSE)

library(jiebaR)   
library(stringr) 

#NLP
engine = worker()
word=segment(business$categories,engine)
word=word[nchar(word1)>1]
word_cat=table(word)
word_cat_sort=sort(word_cat, decreasing = TRUE)[1:40]
word_cat_sort

#choose Breakfast restaurant as our goal
#get the row number of Breakfast in business.json
BreakfastID=vector()
for(i in 1:nrow(business)){
  word=segment(business[i,]$categories,engine)
  if("Breakfast" %in% word & "Restaurants" %in% word){
    BreakfastID=c(BreakfastID,i)
  }
}
#filter business.json
BreakfastData=business[BreakfastID,]

#get the row number of Breakfast in review.json by business_id
Breakfast_review_ID=vector()
for(i in 1:nrow(review)){
  if(review[i,"business_id"] %in% BreakfastData$business_id){
    Breakfast_review_ID=c(Breakfast_review_ID,i)
  }
  if(i%%100==0) print(i)
}
#filter review.json
Breakfast_review=review[Breakfast_review_ID,]

#write table
write.table(BreakfastData,"BreakfastData.tsv",row.names=FALSE,sep="\t")
write.csv(Breakfast_review,"Breakfast_review.csv",row.names=FALSE)





