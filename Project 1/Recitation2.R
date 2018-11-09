###                                             ###
### 15.459 Financial Data Science and Computing ###
###             Youssef Berrada                 ###
###             yberrada@mit.edu                ###
###                Fall 2018                    ###
library(tm)
library(RTextTools)
library(Rgr)

#1. Data Acquisition

require(tm.corpus.Reuters21578)
data(Reuters21578)
rt = Reuters21578


ID(rt[[1]])                     
meta(rt[[1]])                   
show(rt)                        
summary(rt)                     
inspect(rt)  


#2. Data-Preprocessing

clean.rt <- function(rt) {
  rtlower <- tm_map(rt, content_transformer(tolower))
  rtnum <- tm_map(rtlower, removeNumbers)
  rtpun <- tm_map(rtnum, removePunctuation)
  rtsw <- tm_map(rtpun, removeWords, stopwords("english"))
  rtownSW <- tm_map(rtsw, removeWords, c("", ""))
  rt <- tm_map(rt, removeWords, "said") 
  rt <- tm_map(rt, removeWords, "and") 
  rtWs <- tm_map(rtownSW, stripWhitespace)
  rt_Doc <- tm_map(rtWs,stemDocument)
  
  clean_corpus <- rt_Doc
  
  return(clean_corpus)
}

clean_corpus <- clean.rt(rt)


#given topics
topics <- c("earn","ship")
clean_dtm <-DocumentTermMatrix(clean_corpus)




#3. Data Analysis

labelFUN <- function(topic, rt) {
  label <- list()
  for (i in 1:length(topic)){
    label[[i]] <- rep(0, length(rt))
    
    for (j in 1:length(rt)) {
      if (topic[i] %in% meta(rt[[j]], tag = 'topics_cat')) {
        label[[i]][j] <- 1
      }
    }
  }
  
  
  ldafr <- do.call(cbind.data.frame, label)
  names(ldafr) <- topic
  return(ldafr)
}



#Dataframe for class labels
labelDF <- labelFUN(topics, rt)



container <- list()
analytics <- list()
for(i in 1:length(topics)){
  #container[[i]] <- create_container(clean_dtm, labelDF[rownames(clean_dtm), i], trainSize = 1:12850, testSize= 12850:19049, virgin=FALSE)
  container[[i]] <- create_container(clean_dtm, labelDF[rownames(clean_dtm), i], trainSize = 1:100, testSize= 100:190, virgin=FALSE)
  
  #Train
  SVM <- train_model(container[[i]], "SVM")
  
  #Classify
  SVMCL <- classify_model(container[[i]], SVM)
  
  #Analytics
  analytics[[i]] <- create_analytics(container[[i]], cbind(SVMCL))
}




#Evaluation criteia 
Precision <-c()
Recall <- c()
F1Score <- c()

#SVM Accuracy dataframe
for(i in 1:length(topics)) {
  Precision[i] <- summary(analytics[[i]])[1]
  Recall[i] <- summary(analytics[[i]])[2]
  F1Score[i] <- summary(analytics[[i]])[3]
}
scoreSVM <- data.frame(topics, Precision, Recall, F1Score)

topic_summary <- analytics[[1]]@label_summary
alg_summary <-  analytics[[1]]@algorithm_summary
ens_summary <- analytics[[1]]@ensemble_summary
doc_summary <-  analytics[[1]]@document_summary

#Visualise
library(ggplot2)
#Plot SVM Measures
plotSVM <- data.frame(scoreSVM[1], stack(scoreSVM[2:4]))
colnames(plotSVM) <- c("Topics", "Measures", "Type")
ggplot(data=plotSVM, aes(x=Topics, y=Measures, group=Type, color=Type))+ geom_point()



# Tuning 

# Create Trem-document matrix
docmex <- DocumentTermMatrix(rt)
Dtrim <- dim(docmex)


# TF-IDF
library(slam)
#Train Set
idf <- tapply(docmex$v/row_sums(docmex)[docmex$i], docmex$j, mean) * log2(nDocs(docmex)/col_sums(docmex > 0))
summary(idf)
#plot(idf)

docmex <- docmex[,idf >= 0.995]
docmex <- docmex[row_sums(docmex) > 0,]
dim(docmex)

#Remove SparseTerms
rtdm <- removeSparseTerms(docmex, 0.995)
dim(rtdm)


#Remove Sparse Terms
docmex <- removeSparseTerms(docmex, 0.1)
dim(docmex)



