library(jiebaR)   
library(stringr)
library(jiebaR)   
library(stringr) 
library(text2vec) 
library(ggplot2)
library(dplyr)

##load dataset
BreakfastData=read.table("BreakfastData.tsv",sep = "\t",header=T)
Breakfast_review=read.csv("../../Breakfast_review.csv",header=T)


##use jiebaR library
##itoken
tok_fun = word_tokenizer   
it_train = itoken(Breakfast_review$text, 
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
pruned_vocab_key=pruned_vocab_sort[c(6,9,14,17,22,27,29,31,32,42,45,47,57,66,67,71,72,73,76,81,82,83,88,
                                     101,123,124,127,128,133,140,151,154,159,161,165,173,180,181,183,188,
                                     207,209,213,223,230,236,239,240,242,254,263,264,265,276,282,
                                     289,295,296,297,301,305,316,325,339,342,346,347,348,356,361,363,368,
                                     382,393,399,400,407,410,416,429,451,452,453,459,482,484,485,486,488),]

##form word frequency matrix
vectorizer=vocab_vectorizer(pruned_vocab_key)
dtm_train=create_dtm(it_train,vectorizer)
dtm_train=as.matrix(dtm_train)
dtm_train=as.data.frame(dtm_train)

##stem words
dtm_train["biscuit"]=dtm_train["biscuit"]+dtm_train["biscuits"]
dtm_train["burger"]=dtm_train["burger"]+dtm_train["burgers"]
dtm_train["cafe"]=dtm_train["cafe"]+dtm_train["Cafe"]
dtm_train["cafe"]=dtm_train["cafe"]+dtm_train["Coffee"]
dtm_train["cafe"]=dtm_train["cafe"]+dtm_train["coffee"]
dtm_train["chicken"]=dtm_train["chicken"]+dtm_train["Chicken"]
dtm_train["drink"]=dtm_train["drink"]+dtm_train["drinks"]
dtm_train["egg"]=dtm_train["egg"]+dtm_train["eggs"]+dtm_train["Eggs"]
dtm_train["fried"]=dtm_train["fried"]+dtm_train["fries"]
dtm_train["omelette"]=dtm_train["omelette"]+dtm_train["omelet"]
dtm_train["potato"]=dtm_train["potato"]+dtm_train["potatoes"]
dtm_train["salad"]=dtm_train["salad"]+dtm_train["salads"]
dtm_train["sandwich"]=dtm_train["sandwich"]+dtm_train["sandwiches"]
dtm_train["service"]=dtm_train["service"]+dtm_train["Service"]
dtm_train["waffle"]=dtm_train["waffle"]+dtm_train["waffles"]
dtm_train["wait"]=dtm_train["wait"]+dtm_train["waited"]+dtm_train["waiting"]
dtm=select(dtm_train,-c(biscuits,burgers,Cafe,Coffee,coffee,Chicken,drinks,eggs,Eggs,fries,omelet
                      ,potatoes,salads,sandwiches,Service,waffles,waited,waiting))
dtm[dtm>0]=1
##add stars feature
dtm["stars"]=Breakfast_review["stars"]
dtm=dtm[grepl("^[[:digit:]]+$",dtm$stars),]


colnames(dtm)
for(i in colnames(dtm)[1]){
  keyword_stars=dtm[dtm[i]==1,c(i,"stars")]
  ggplot(keyword_stars)+
    geom_bar(aes(x=stars),stat = "count", width=0.9, position="stack")
}


ggplot(keyword_stars)+
  geom_bar(aes(x=stars),stat = "count", width=0.9, position="stack")





write.csv(dtm,"dtm.csv",row.names=FALSE)

