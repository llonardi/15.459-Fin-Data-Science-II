rm(list=ls())

#install.packages(c("mnormt", "psych", "SnowballC", "hunspell", 
#                   "broom", "tokenizers", "janeaustenr"))
#install.packages("tidytext")
#install.packages("e1071")
#install.packages("RTextTools")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("slam")
#install.packages("nnet")
#library(stringr)
#library(tidyquant)
library(RTextTools)
library(tm)
library(slam)
library(nnet)
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
# SVM vanilla running
SVM <- train_model(container,"SVM")
SVM_CLASSIFY <- classify_model(container, SVM)

# SVM one against the rest
labelFUN <- function(topic, rt) {
  label <- list()
  for (i in 1:length(topic)){
    label[[i]] <- rep(0, length(rt$cat))
    
    for (j in 1:length(rt$cat)) {
      if (topic[i] %in% rt$cat[j]) {
        label[[i]][j] <- 1
      }
    }
  }
  
  
  ldafr <- do.call(cbind.data.frame, label)
  names(ldafr) <- topic
  return(ldafr)
}

labelDF_multi <- labelFUN(h1, parent_totalND)

train_matrix_corpus_multi = VCorpus(VectorSource(parent_totalND$article))
train_matrix_total_multi = DocumentTermMatrix(train_matrix_corpus_multi)

container_multi <- list()
analytics_multi <- list()
for(i in 1:length(h1)){
  container_multi[[i]] <- create_container(train_matrix_total_multi,
                                     labelDF_multi[rownames(train_matrix_total_multi), i],
                                     trainSize = 1:1000,
                                     testSize= 1001:2000,
                                     virgin=FALSE)
  #Train
  SVM_multi <- train_model(container_multi[[i]], "SVM")
  #Classify
  SVMCL_multi <- classify_model(container_multi[[i]], SVM_multi)
  #Analytics
  analytics_multi[[i]] <- create_analytics(container_multi[[i]], cbind(SVMCL_multi))
}

Precision_multi <-c()
Recall_multi <- c()
F1Score_multi <- c()

for(i in 1:length(h1)) {
  Precision_multi[i] <- summary(analytics_multi[[i]])[1]
  Recall_multi[i] <- summary(analytics_multi[[i]])[2]
  F1Score_multi[i] <- summary(analytics_multi[[i]])[3]
}
scoreSVM_multi <- data.frame(h1, Precision_multi, Recall_multi, F1Score_multi)
print(scoreSVM_multi)


# Other methods
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

# Create a data set with duplicates
batch_size = 250
parent_test_duplicate = parent_test[1:(4*batch_size),]
parent_train$cat <- trimws(parent_train$cat) 
parent_train_CCAT = parent_train[parent_train$cat %in% "CCAT",][1:batch_size,]
parent_train_ECAT = parent_train[parent_train$cat %in% "ECAT",][1:batch_size,]
parent_train_GCAT = parent_train[parent_train$cat %in% "GCAT",][1:batch_size,]
parent_train_MCAT = parent_train[parent_train$cat %in% "MCAT",][1:batch_size,]

parent_train_total = rbind(parent_train_CCAT,
                             parent_train_ECAT,
                             parent_train_GCAT,
                             parent_train_MCAT)
rownames(parent_train_total) = NULL # resetting row numbers

# Create a data set with both train and test data sets
parent_total_duplicate <- rbind(parent_train_total, parent_test_duplicate)
parent_total_duplicate$cat <- trimws(parent_total_duplicate$cat)

#Document Term Matrix
matrix_duplicate <- create_matrix(parent_total_duplicate$article,
                            language="english",
                            removeNumbers=TRUE,
                            stemWords=TRUE,
                            removeSparseTerms=.998)

#Container
container_duplicate <- create_container(matrix_duplicate,
                              as.numeric(factor(parent_total_duplicate$cat)),
                              trainSize=1:(4*batch_size),
                              testSize=(4*batch_size+1):length(parent_total_duplicate$cat),
                              virgin=FALSE)  

# Model training
BAGGING_duplicate <- train_model(container_duplicate,"BAGGING")
BAGGING_duplicate_CLASSIFY <- classify_model(container_duplicate, BAGGING_duplicate)

NNET_duplicate <- train_model(container_duplicate,"NNET")
NNET_duplicate_CLASSIFY <- classify_model(container_duplicate, NNET_duplicate)

# Check if NNET and BAGGING take care of multiple classifications by default --> answer: NO
control_df_BAGGING = cbind(BAGGING_duplicate_CLASSIFY,parent_test_duplicate$id,parent_test_duplicate$cat)
control_df_NNET = cbind(NNET_duplicate_CLASSIFY,parent_test_duplicate$id,parent_test_duplicate$cat)

# Model training with allowing multiple classifications
BAGGING_duplicate_multiple <- train_model(container_duplicate,"BAGGING")
BAGGING_duplicate_multiple_CLASSIFY <- classify_model(container_duplicate, BAGGING_duplicate_multiple)

NNET_multiple_duplicate <- train_model(container_duplicate,"NNET")
NNET_duplicate_multiple_CLASSIFY <- classify_model(container_duplicate, NNET_duplicate_multiple)

# ------------- NOT WORKING
#Manual NNet
set_sparse = 0.7
mycorpus <- VCorpus(VectorSource(parent_train_total$article))
train_matrix = DocumentTermMatrix(mycorpus)
train_matrix_sparse = removeSparseTerms(train_matrix,
                                        sparse = set_sparse)

my_corpus_test = VCorpus(VectorSource(parent_test_duplicate$article))
train_matrix_test = DocumentTermMatrix(my_corpus_test)
train_matrix_test_sparse = removeSparseTerms(train_matrix_test,
                                             sparse = set_sparse)

nnet_docmex = nnet(train_matrix_sparse,
                   as.numeric(factor(parent_train_total$cat)),
                   size = 4,
                   MaxNWts = 60000,
                   censored = FALSE)

prediction = predict(nnet_docmex,train_matrix_test_sparse)
# --------------

# Creating SVM loop

labelFUN <- function(topic, rt) {
  label <- list()
  for (i in 1:length(topic)){
    label[[i]] <- rep(0, length(rt$cat))
    
    for (j in 1:length(rt$cat)) {
      if (topic[i] %in% rt$cat[j]) {
        label[[i]][j] <- 1
      }
    }
  }
  
  
  ldafr <- do.call(cbind.data.frame, label)
  names(ldafr) <- topic
  return(ldafr)
}

labelDF <- labelFUN(h1, parent_total_duplicate)

train_matrix_corpus = VCorpus(VectorSource(parent_total_duplicate$article))
train_matrix_total = DocumentTermMatrix(train_matrix_corpus)

container <- list()
analytics <- list()
for(i in 1:length(h1)){
  container[[i]] <- create_container(train_matrix_total,
                                     labelDF[rownames(train_matrix_total), i],
                                     trainSize = 1:1000,
                                     testSize= 1001:2000,
                                     virgin=FALSE)
  #Train
  SVM <- train_model(container[[i]], "SVM")
  #Classify
  SVMCL <- classify_model(container[[i]], SVM)
  #Analytics
  analytics[[i]] <- create_analytics(container[[i]], cbind(SVMCL))
}

Precision <-c()
Recall <- c()
F1Score <- c()

for(i in 1:length(h1)) {
  Precision[i] <- summary(analytics[[i]])[1]
  Recall[i] <- summary(analytics[[i]])[2]
  F1Score[i] <- summary(analytics[[i]])[3]
}
scoreSVM <- data.frame(h1, Precision, Recall, F1Score)
print(scoreSVM)
