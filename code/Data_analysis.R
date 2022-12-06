library(jiebaR)   
library(stringr) 
library(text2vec) 
library(ggplot2)
library(dplyr)

##load dataset
BreakfastData=read.table("BreakfastData.tsv",sep = "\t",header=T)
Breakfast_review=read.csv("Breakfast_review.csv",header=T,encoding = "UTF-8")
review_information=read.csv("review_stars.csv",header=T)
user_important=read.csv("important_user0.csv",header=T)

#get important review
review_important=review_information[which(review_information$user_id %in% user_important$user_id),]
#get important breakfast review
Breakfast_review_important=Breakfast_review[which(Breakfast_review$user_id %in% user_important$user_id),]


##use jiebaR library
##itoken
tok_fun = word_tokenizer   
it_train = itoken(Breakfast_review_important$text, 
                  tokenizer = tok_fun, 
                  progressbar = FALSE)
##create_vocabulary
stop_words <- read.table("stop_words_english.txt")
vocab = create_vocabulary(it_train,stopwords = stop_words$V1)

##remove some words
vocab=vocab[nchar(vocab$term)>2,]
vocab=vocab[!grepl('[0-9\\._+]',vocab$term),] 
pruned_vocab = prune_vocabulary(vocab, 
                                term_count_min = 10, 
                                doc_proportion_min = 0.001)

#sort words
pruned_vocab_sort=pruned_vocab[order(pruned_vocab[,3],decreasing = T),]
rownames(pruned_vocab_sort)=1:nrow(pruned_vocab_sort)

#choose key words
pruned_vocab_key=pruned_vocab_sort[c(2,10,21,23,28,29,32,38,42,49,51,53,55,60,62,65,71,72,73,77,83,109,113,114,131,132,134,138,156,168,172,197,198,200,
                                     6,15,18,22,47,78,84,97,135,160,164,186,
                                     111,124),]

##form word frequency matrix
vectorizer=vocab_vectorizer(pruned_vocab_key)
dtm_train=create_dtm(it_train,vectorizer)
dtm_train=as.matrix(dtm_train)
dtm_train=as.data.frame(dtm_train)

##stem words
dtm_train["food_related"]=apply(dtm_train[,c(1:34)],1,sum)
dtm_train["service_related"]=apply(dtm_train[,c(35:46)],1,sum)
dtm_train["price_related"]=apply(dtm_train[,c(47:48)],1,sum)
dtm_train[dtm_train>0]=1

dtm=select(dtm_train,c("food_related","service_related","price_related"))

##get users' historical rating information
means=aggregate(review_important$stars,by=list(type=review_important$user_id),mean)
vars=aggregate(review_important$stars,by=list(type=review_important$user_id),var)

user_star=cbind(means,vars)
user_star[is.na(user_star)]=0
user_star=as.data.frame(user_star)
colnames(user_star)=c("user_id","mean","user_id","var")
user_star$var=user_star$var+0.001

##get adjust stars
Breakfast_review_important["ind"] = match(t(Breakfast_review_important["user_id"]), t(user_star["user_id"]))
Breakfast_review_important["stars_adjust"]=(Breakfast_review_important["stars"]-user_star[Breakfast_review_important$ind,2])/sqrt(user_star[Breakfast_review_important$ind,4])

##add stars feature
dtm["stars"]=Breakfast_review_important["stars"]
dtm["stars_adjust"]=Breakfast_review_important["stars_adjust"]
dtm["business_id"]=Breakfast_review_important["business_id"]

#scale_score=function(x){
#  if(x< -0.75){y=1}
#  else if(x>=-0.75&x< -0.25){y=2}
#  else if(x>=-0.25&x<0.25){y=3}
#  else if(x>=0.25&x<0.75){y=4}
#  else{y=5}
#  return(y)
#}

#dtm["stars_scale2"]=apply(dtm['stars_adjust'],1,scale_score)

#write.csv(dtm_train,"dtm.csv",row.names=FALSE)




