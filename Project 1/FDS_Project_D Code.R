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
Parent_train <- read.csv("/Users/hermannviktor/Dropbox/MIT/Courses/2. Fall Term/15.458 Data Science/Assignments/Assignment 4/H1_Data.csv",sep=";")
Child_train <- read.csv("/Users/hermannviktor/Dropbox/MIT/Courses/2. Fall Term/15.458 Data Science/Assignments/Assignment 4/Heirarchies_Data.csv")
testerBitch <- read.csv("/Users/hermannviktor/Dropbox/MIT/Courses/2. Fall Term/15.458 Data Science/Assignments/Assignment 4/Data_proj_D_test.csv")

# Slicing testerBitch into H1 and lower classes

testerBitch$cat <- trimws(testerBitch$cat) 

h1 = c("GCAT","CCAT","ECAT","MCAT")

testerBitch$cat <- as.character(testerBitch$cat)
parent_test <- testerBitch[testerBitch$cat %in% h1,]  #%in% c("GCAT","CCAT","ECAT","MCAT"),]
child_test <- testerBitch[!testerBitch$cat %in% h1,]

colnames(Parent_train) = c("cat", "id", "article")

parent_total <- rbind(Parent_train, parent_test)

parent_total$cat <- trimws(parent_total$cat) 

#Document Term Matrix
doc_matrix <- create_matrix(parent_total$article[1:4000], language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms=.998)

#Container
container <- create_container(doc_matrix,as.numeric(factor(parent_total$cat)), trainSize=1:2000,
                              testSize=2001:4000, virgin=FALSE)  

#Training Models
SVM <- train_model(container,"SVM")
SVM_CLASSIFY <- classify_model(container, SVM) #3 MANGOOOOO

SLDA <- train_model(container,"SLDA")
SLDA_CLASSIFY <- classify_model(container, SLDA) #2 Laura

BAGGING <- train_model(container,"BAGGING")
BAGGING_CLASSIFY <- classify_model(container, BAGGING)#very sexual tbh

NNET <- train_model(container,"NNET")
NNET_CLASSIFY <- classify_model(container, NNET) #1

GLMNET = train_model(container,"GLMNET")
GLMNET_CLASSIFY = classify_model(container,GLMNET)

MAXENT = train_model(container, "MAXENT")
MAXENT_CLASSIFY = classify_model(container,MAXENT)

BOOSTING = train_model(container, "BOOSTING")
BOOSTING_CLASSIFY = classify_model(container, BOOSTING)

analytics_total = create_analytics(container, 
                                   cbind(SVM_CLASSIFY,SLDA_CLASSIFY,BAGGING_CLASSIFY, NNET_CLASSIFY,GLMNET_CLASSIFY, MAXENT_CLASSIFY, BOOSTING_CLASSIFY))
summary(analytics_total)


# 6.Cross-Validation
SVM <- cross_validate(container, 4, "SVM")
SLDA <- cross_validate(container, 4, "SLDA")
BAGGING <- cross_validate(container, 4, "BAGGING")
NNET <- cross_validate(container, 4, "NNET")


# 7.Exporting Data
write.csv(analytics@document_summary, "DocumentSummary.csv")


