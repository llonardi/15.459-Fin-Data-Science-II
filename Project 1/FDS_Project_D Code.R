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

newsDF <- read.csv("Proj4.csv")

#Document Term Matrix
doc_matrix <- create_matrix(newsDF$article, language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms=.998)

#Container
container <- create_container(doc_matrix, newsDF$cat, trainSize=1:26000,
                              testSize=26001:27087, virgin=FALSE)

#Training Models
SVM <- train_model(container,"SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
SLDA <- train_model(container,"SLDA")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")
TREE <- train_model(container,"TREE")

#Classifications
SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
SLDA_CLASSIFY <- classify_model(container, SLDA)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)
RF_CLASSIFY <- classify_model(container, RF)
NNET_CLASSIFY <- classify_model(container, NNET)
TREE_CLASSIFY <- classify_model(container, TREE)

# 5.Analytics
analytics <- create_analytics(container,
                              cbind(SVM_CLASSIFY))

summary(analytics)
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <-analytics@ensemble_summary
doc_summary <- analytics@document_summary

# 6.Cross-Validation
SVM <- cross_validate(container, 4, "SVM")

# 7.Exporting Data
write.csv(analytics@document_summary, "DocumentSummary.csv")


