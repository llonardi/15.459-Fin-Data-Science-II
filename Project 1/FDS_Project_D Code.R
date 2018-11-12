rm(list=ls())

#install.packages(c("mnormt", "psych", "SnowballC", "hunspell", 
#                   "broom", "tokenizers", "janeaustenr"))
#install.packages("tidytext")
#install.packages("e1071")
#install.packages("RTextTools")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("slam")
library(stringr)
library(tidyquant)
library(RTextTools)
library(tm)
library(SnowballC)
library(broom)
library(pdftools)
library(tau)
library(wordcloud)

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
analytics_total = create_analytics(container, cbind(SVM_CLASSIFY, SLDA_CLASSIFY, MAXENT_CLASSIFY,GLMNET_CLASSIFY))
summary(analytics_total)

n_times_fold = 4
SVM <- cross_validate(container, n_times_fold, "SVM")
SLDA <- cross_validate(container, n_times_fold, "SLDA")
MAXENT <- cross_validate(container, n_times_fold, "MAXENT")
GLMNET <- cross_validate(container, n_times_fold, "GLMNET")
BAGGING <- cross_validate(container, n_times_fold, "BAGGING")
NNET <- cross_validate(container, n_times_fold, "NNET")

# Training models - model tuning
LGLMNET=train_model(container,"GLMNET",maxitglm = 10^6)
LGLMNET_CLASSIFY = classify_model(container,LGLMNET)

Y1_MAXENT = train_model(container, "MAXENT", l1_regularizer = 1, use_sgd = TRUE)
Y1_MAXENT_CLASSIFY = classify_model(container,Y1_MAXENT)

Y2_MAXENT = train_model(container, "MAXENT", l2_regularizer = 1)
Y2_MAXENT_CLASSIFY = classify_model(container,Y2_MAXENT)

Y3_MAXENT = train_model(container, "MAXENT", set_heldout = 0.2*8*test_batch)
Y3_MAXENT_CLASSIFY = classify_model(container,Y3_MAXENT)

#Ensemble Analytics

#MAXENT and GLMNET
MGanalytics_total = create_analytics(container, cbind(MAXENT_CLASSIFY,GLMNET_CLASSIFY))
summary(MGanalytics_total)

#MAXENT and SVM
MSanalytics_total = create_analytics(container, cbind(MAXENT_CLASSIFY,SVM_CLASSIFY))
summary(MSanalytics_total)

#SVM and GLMNET
SGanalytics_total = create_analytics(container, cbind(SVM_CLASSIFY,GLMNET_CLASSIFY))
summary(SGanalytics_total)

#Fine-Tune Analytics
Y1analytics_total = create_analytics(container, Y1_MAXENT_CLASSIFY)
summary(Y1analytics_total)

Y2analytics_total = create_analytics(container, Y2_MAXENT_CLASSIFY)
summary(Y2analytics_total)

Y3analytics_total = create_analytics(container, Y3_MAXENT_CLASSIFY)
summary(Y3analytics_total)

Lanalytics_total = create_analytics(container, LGLMNET_CLASSIFY)
summary(Lanalytics_total)

EnsembleAnal_total=create_analytics(container, cbind(Y3_MAXENT_CLASSIFY,LGLMNET_CLASSIFY))
summary(EnsembleAnal_total)

# 7.Exporting Data
write.csv(analytics@document_summary, "DocumentSummary.csv")


