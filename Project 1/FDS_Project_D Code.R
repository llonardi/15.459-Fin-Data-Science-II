rm(list=ls())

install.packages(c("mnormt", "psych", "SnowballC", "hunspell", 
                   "broom", "tokenizers", "janeaustenr"))
install.packages("tidytext")
install.packages("e1071")
install.packages("RTextTools")
install.packages("tm")
install.packages("wordcloud")
install.packages("slam")
library(stringr)
library(tidyquant)
library(RTextTools)
library(tm)
library(SnowballC)
library(broom)
library(pdftools)
library(tau)
library(wordcloud)

# Read in data
Parent_train <- read.csv("/Users/hermannviktor/Dropbox/MIT/Courses/2. Fall Term/15.458 Data Science/Assignments/Assignment 4/H1_Data.csv",sep=";",row.names = NULL)
Child_train <- read.csv("/Users/hermannviktor/Dropbox/MIT/Courses/2. Fall Term/15.458 Data Science/Assignments/Assignment 4/Heirarchies_Data.csv",row.names = NULL)
testerBitch <- read.csv("/Users/hermannviktor/Dropbox/MIT/Courses/2. Fall Term/15.458 Data Science/Assignments/Assignment 4/Data_proj_d_test.csv", row.names = NULL)

# Slicing testerBitch into H1 and lower classes

testerBitch$cat <- trimws(testerBitch$cat) 

h1 = c("GCAT","CCAT","ECAT","MCAT")

testerBitch$cat <- as.character(testerBitch$cat)
parent_test <- testerBitch[testerBitch$cat %in% h1,]  #%in% c("GCAT","CCAT","ECAT","MCAT"),]
child_test <- testerBitch[!testerBitch$cat %in% h1,]

colnames(Parent_train) = c("cat", "id", "article")

parent_total <- rbind(Parent_train, parent_test)

parent_total$cat <- trimws(parent_total$cat) 

head(parent_total$cat)
#Document Term Matrix
doc_matrix <- create_matrix(parent_total$article[1:400], language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms=.998)

#Container
container <- create_container(doc_matrix, parent_total$cat, trainSize=1:200,
                              testSize=201:400, virgin=FALSE)  ###############

#Training Models
SVM <- train_model(container,"SVM")
SLDA <- train_model(container,"SLDA")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")
TREE <- train_model(container,"TREE")

#Classifications
SVM_CLASSIFY <- classify_model(container, SVM) #3 MANGOOOOO
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
SLDA_CLASSIFY <- classify_model(container, SLDA) #2 Laura
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)#very sexual tbh
RF_CLASSIFY <- classify_model(container, RF) #possibly sexual
NNET_CLASSIFY <- classify_model(container, NNET) #1
TREE_CLASSIFY <- classify_model(container, TREE)

# 5.Analytics
analytics_SVM <- create_analytics(container,
                                  cbind(SVM_CLASSIFY))
summary(analytics_SVM)


analytics_SLDA <- create_analytics(container,
                                   cbind(SLDA_CLASSIFY))
summary(analytics_SLDA)


analytics_BAGGING <- create_analytics(container,
                                      cbind(BAGGING_CLASSIFY))
summary(analytics_BAGGING)


analytics_NNET <- create_analytics(container,
                                   cbind(NNET_CLASSIFY))

summary(analytics_NNET)

analytics_total <- create_analytics(container,
                                   cbind(SVM_CLASSIFY, BAGGING_CLASSIFY))

summary(analytics_total)


#Evaluation criteia 
Precision_SVM <-c()
Recall_SVM <- c()
F1Score_SVM <- c()

Precision_SLDA <-c()
Recall_SLDA <- c()
F1Score_SLDA <- c()

Precision_BAGGING <-c()
Recall_BAGGING <- c()
F1Score_BAGGING <- c()

Precision_NNET <-c()
Recall_NNET <- c()
F1Score_NNET <- c()


#SVM Accuracy dataframe
for(i in 1:length(h1)) {
  Precision_SVM[i] <- summary(analytics_SVM[[i]])[1]
  Recall_SVM[i] <- summary(analytics_SVM[[i]])[2]
  F1Score_SVM[i] <- summary(analytics_SVM[[i]])[3]
  
  Precision_SLDA[i] <- summary(analytics_SLDA[[i]])[1]
  Recall_SLDA[i] <- summary(analytics_SLDA[[i]])[2]
  F1Score_SLDA[i] <- summary(analytics_SLDA[[i]])[3]
  
  Precision_BAGGING[i] <- summary(analytics_BAGGING[[i]])[1]
  Recall_BAGGING[i] <- summary(analytics_BAGGING[[i]])[2]
  F1Score_BAGGING[i] <- summary(analytics_BAGGING[[i]])[3]
  
  Precision_NNET[i] <- summary(analytics_NNET[[i]])[1]
  Recall_NNET[i] <- summary(analytics_NNET[[i]])[2]
  F1Score_NNET[i] <- summary(analytics_NNET[[i]])[3] 
}


score_SVM <- data.frame(topics_SVM, Precision_SVM, Recall_SVM, F1Score_SVM)
score_SLDA <- data.frame(topics_SLDA, Precision_SLDA, Recall_SLDA, F1Score_SLDA)
score_BAGGING <- data.frame(topics_BAGGING, Precision_BAGGING, Recall_BAGGING, F1Score_BAGGING)
score_NNET <- data.frame(topics_NNET, Precision_NNET, Recall_NNET, F1Score_NNET)

#topic_summary <- analytics@label_summary
#alg_summary <- analytics@algorithm_summary
#ens_summary <-analytics@ensemble_summary
#doc_summary <- analytics@document_summary

# 6.Cross-Validation
SVM <- cross_validate(container, 4, "SVM")
SLDA <- cross_validate(container, 4, "SLDA")
BAGGING <- cross_validate(container, 4, "BAGGING")
NNET <- cross_validate(container, 4, "NNET")


# 7.Exporting Data
write.csv(analytics@document_summary, "DocumentSummary.csv")