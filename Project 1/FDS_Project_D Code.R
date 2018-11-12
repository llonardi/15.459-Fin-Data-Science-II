rm(list=ls())

#install.packages(c("mnormt", "psych", "SnowballC", "hunspell", 
#                   "broom", "tokenizers", "janeaustenr"))
#install.packages("tidytext")
#install.packages("e1071")
#install.packages("RTextTools")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("slam")
#library(stringr)
#library(tidyquant)
library(RTextTools)
library(tm)
library(slam)
#library(SnowballC)
#library(broom)
#library(pdftools)
#library(tau)
#library(wordcloud)

#filepaths
filepath_parent_train = "/Users/hermannviktor/Dropbox/MIT/Courses/2. Fall Term/15.458 Data Science/Assignments/Assignment 4/H1_Data.csv"
filepath_child_train = "/Users/hermannviktor/Dropbox/MIT/Courses/2. Fall Term/15.458 Data Science/Assignments/Assignment 4/Heirarchies_Data.csv"
filepath_testerBitch = "/Users/hermannviktor/Dropbox/MIT/Courses/2. Fall Term/15.458 Data Science/Assignments/Assignment 4/Data_proj_D_test.csv"

# Read in data
parent_train <- read.csv(filepath_parent_train,sep=";")
child_train <- read.csv(filepath_child_train)
testerBitch <- read.csv(filepath_testerBitch)

# Slicing testerBitch into H1 and lower classes
testerBitch$cat <- trimws(testerBitch$cat) 
h1 = c("GCAT","CCAT","ECAT","MCAT")
testerBitch$cat <- as.character(testerBitch$cat)

parent_test <- testerBitch[testerBitch$cat %in% h1,]  #%in% c("GCAT","CCAT","ECAT","MCAT"),]
child_test <- testerBitch[!testerBitch$cat %in% h1,]

colnames(parent_train) = c("cat", "id", "article")
parent_total <- rbind(parent_train, parent_test)
parent_total$cat <- trimws(parent_total$cat) 

#No Duplicate Data
parent_trainND = parent_train[!duplicated(parent_train$id),]
parent_testND = parent_test[!duplicated(parent_test$id),][1:(4*batch_size),]

# Creating batches to make sure that there is of each category enough data points for training set
batch_size = 250
parent_trainND$cat <- trimws(parent_trainND$cat) 
parent_trainND_CCAT = parent_trainND[parent_trainND$cat %in% "CCAT",][1:batch_size,]
parent_trainND_ECAT = parent_trainND[parent_trainND$cat %in% "ECAT",][1:batch_size,]
parent_trainND_GCAT = parent_trainND[parent_trainND$cat %in% "GCAT",][1:batch_size,]
parent_trainND_MCAT = parent_trainND[parent_trainND$cat %in% "MCAT",][1:batch_size,]

parent_trainND_total = rbind(parent_trainND_CCAT,
                             parent_trainND_ECAT,
                             parent_trainND_GCAT,
                             parent_trainND_MCAT)
rownames(parent_trainND_total) = NULL # resetting row numbers

# Create a data set with both train and test data sets
parent_totalND <- rbind(parent_trainND_total, parent_testND)
parent_totalND$cat <- trimws(parent_totalND$cat)

#Document Term Matrix
doc_matrix <- create_matrix(parent_totalND$article,
                            language="english",
                            removeNumbers=TRUE,
                            stemWords=TRUE,
                            removeSparseTerms=.998)

#Container
container <- create_container(doc_matrix,
                              as.numeric(factor(parent_totalND$cat)),
                              trainSize=1:(4*batch_size),
                              testSize=(4*batch_size+1):length(parent_totalND$cat),
                              virgin=FALSE)  

#Training Models - Initial tests
SVM <- train_model(container,"SVM")
SVM_CLASSIFY <- classify_model(container, SVM)

SLDA <- train_model(container,"SLDA")
SLDA_CLASSIFY <- classify_model(container, SLDA)

BAGGING <- train_model(container,"BAGGING")
BAGGING_CLASSIFY <- classify_model(container, BAGGING)

NNET <- train_model(container,"NNET")
NNET_CLASSIFY <- classify_model(container, NNET)

GLMNET = train_model(container,"GLMNET")
GLMNET_CLASSIFY = classify_model(container,GLMNET)

MAXENT = train_model(container, "MAXENT")
MAXENT_CLASSIFY = classify_model(container,MAXENT)

#Total Analytics
analytics_total = create_analytics(container,
                                   cbind(SVM_CLASSIFY, SLDA_CLASSIFY, MAXENT_CLASSIFY,GLMNET_CLASSIFY,BAGGING_CLASSIFY,NNET_CLASSIFY))
summary(analytics_total)

# Cross-validation - Do NOT run unless necessary (takes ages)
n_times_fold = 4
SVM <- cross_validate(container, n_times_fold, "SVM")
SLDA <- cross_validate(container, n_times_fold, "SLDA")
MAXENT <- cross_validate(container, n_times_fold, "MAXENT")
GLMNET <- cross_validate(container, n_times_fold, "GLMNET")
BAGGING <- cross_validate(container, n_times_fold, "BAGGING")
NNET <- cross_validate(container, n_times_fold, "NNET")

# Training models - model selection
analytics_svm_maxent = create_analytics(container,
                                        cbind(SVM_CLASSIFY,MAXENT_CLASSIFY))
summary(analytics_svm_maxent)

# Model tuning
# SVM
change_SVM = train_model(container,
                       "SVM",
                       cost = 0.1,
                       cross = 10)
change_SVM_CLASSIFY = classify_model(container,change_SVM)

analytics_change_SVM = create_analytics(container,
                                      cbind(change_SVM_CLASSIFY))
summary(analytics_cost_SVM)

#Maxent
Y1_MAXENT = train_model(container, "MAXENT", l1_regularizer = 1, use_sgd = TRUE)
Y1_MAXENT_CLASSIFY = classify_model(container,Y1_MAXENT)

Y2_MAXENT = train_model(container, "MAXENT", l2_regularizer = 1)
Y2_MAXENT_CLASSIFY = classify_model(container,Y2_MAXENT)

Y3_MAXENT = train_model(container, "MAXENT", use_sgd = TRUE)
Y3_MAXENT_CLASSIFY = classify_model(container,Y3_MAXENT)

analytics_change_MAXENT = create_analytics(container,
                                        cbind(Y3_MAXENT_CLASSIFY))
summary(analytics_change_MAXENT)

#Ensemble Analytics of Model tuning
#Tsuruoka 2 and original SVM
ensemble_analytics_total = create_analytics(container, cbind(SVM_CLASSIFY,Y2_MAXENT_CLASSIFY))
summary(ensemble_analytics_total)

# ------------------- THIS NEEDS TO BE FINISHED
# Fine-tuning Tf-Idf
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

# ------------------- 



# 7.Exporting Data
write.csv(analytics@document_summary, "DocumentSummary.csv")


