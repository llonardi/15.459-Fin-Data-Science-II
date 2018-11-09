###                                             ###
### 15.459 Financial Data Science and Computing ###
###             Youssef Berrada                 ###
###             yberrada@mit.edu                ###
###                Fall 2018                    ###


#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
#            "cluster", "igraph", "fpc","RCurl","XML")   
#install.packages(Needed, dependencies=TRUE)   


# Load packages
library(stringr)
library(tidyquant)
library(RTextTools)
library(tm)
library(SnowballC)
library(broom)
library(pdftools)
library(tau)
library(wordcloud)
data("NYTimes")

nyt.data <- NYTimes
mycorpus <- VCorpus(VectorSource(nyt.data$Title))


inspect(mycorpus[[2]])

clean_corpus <- function(corpus){
  mycorpus<-tm_map(mycorpus,content_transformer(tolower))
  mycorpus<-tm_map(mycorpus,removePunctuation)
  mycorpus<-tm_map(mycorpus,removeNumbers)
  mycorpus<-tm_map(mycorpus,removeWords,stopwords("english"))
  
  mycorpus<-tm_map(mycorpus,stripWhitespace)
  #mycorpus<-tm_map(mycorpus,PlainTextDocument)
  
  #dictCorpus<-mycorpus
  mycorpus<-tm_map(mycorpus,stemDocument)
  #mycorpus<-tm_map(mycorpus,stemCompletion,dictCorpus)
  return(mycorpus)
}

mycorpus = clean_corpus(mycorpus)

#build Term Document Matrix
myTDM<-TermDocumentMatrix(mycorpus, control=list(minWordLength=1))
myTDMdf<-as.data.frame(as.matrix(t(myTDM)))

#find most frequent terms
findFreqTerms(myTDM,lowfreq=100)

#find associations
findAssocs(myTDM, terms='war', 0.1)

#n-grams analysis
mydf<-data.frame(text=unlist(sapply(mycorpus,'[',"content")),stringAsFactors=FALSE)
mydf$text<-as.character(mydf$text)

ngram_1L<-textcnt(mydf$text,n=1L,method="string")
ngram_t1<-data.frame(counts=unclass(ngram_1L),size=nchar(names(ngram_1L)),text=names(ngram_1L))
n1L<-arrange(ngram_t1,desc(counts))
counts1<-n1L[,c(3,1)]
View(counts1)

ngram_3L<-textcnt(mydf$text,n=3L,method="string")
ngram_t3<-data.frame(counts=unclass(ngram_3L),size=nchar(names(ngram_3L)),text=names(ngram_3L))
n3L<-arrange(ngram_t3,desc(counts))
counts3<-n3L[,c(3,1)]
View(counts3)

#wordcloud
wordcloud(counts1$text,counts1$count,min.freq=50)

#finding the types of articles
#k-means clustering
wss<-(nrow(myTDMdf)-1)*sum(apply(myTDMdf,2,var))
for(i in 1:15){wss[i]<-sum(kmeans(myTDMdf,centers=i)$withinss)}
plot(1:15,wss,type="b",xlab="No. of Clusters",ylab="wss")

k<-kmeans(myTDMdf,5,nstart=20)

groups<-data.frame(k$cluster)
table(groups)

title<-as.data.frame(nyt.data [,"Title"])
finalDF<-as.data.frame(cbind(title,groups))
names(finalDF)<-c("title","group")

x1<-subset(finalDF, group==2)
View(x1)


N      <- nrow(nyt.data)
Ntrain <- 1500
Ntest  <- N - Ntrain
sparseThreshold <- 0.01

nyt.matrix<-create_matrix(nyt.data$Title,  
                          language="english",
                          removeNumbers = TRUE, 
                          stemWords = TRUE,
                          removeSparseTerms=(1-sparseThreshold),
                          weighting = weightTfIdf)

nyt.container <-create_container(nyt.matrix,
                                 nyt.data$Topic.Code,
                                 trainSize = 1:Ntrain, 
                                 testSize = (Ntrain+1):N,
                                 virgin=FALSE)
nyt.SVM <- train_model(nyt.container,"SVM")

nyt.SVM_CLASSIFY <- classify_model(nyt.container, nyt.SVM)

nyt.analytics <- create_analytics(nyt.container,nyt.SVM_CLASSIFY)
summary(nyt.analytics)

