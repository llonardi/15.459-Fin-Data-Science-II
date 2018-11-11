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
Parent_train <- read.csv("H1_Data.csv")
Child_train <- read.csv("Heirarchies_Data.csv")
testerBitch <- read.csv("Proj_D_test.csv")
rm(newsDF)
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
doc_matrix <- create_matrix(parent_total$article, language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms=.998)

#Container
container <- create_container(doc_matrix,as.numeric(factor(parent_total$cat)), trainSize=1:7000,
                              testSize=(length(Parent_train$cat)+1):(length(Parent_train$cat)+7001), virgin=FALSE)  

#Training Models
SVM <- train_model(container,"SVM")
SVM_CLASSIFY <- classify_model(container, SVM) #3 MANGOOOOO

SLDA <- train_model(container,"SLDA")
SLDA_CLASSIFY <- classify_model(container, SLDA) #2 Laura

#BAGGING <- train_model(container,"BAGGING")
#BAGGING_CLASSIFY <- classify_model(container, BAGGING)#very sexual tbh

#NNET <- train_model(container,"NNET")
#NNET_CLASSIFY <- classify_model(container, NNET) #1

GLMNET = train_model(container,"GLMNET")
GLMNET_CLASSIFY = classify_model(container,GLMNET)

LGLMNET=train_model(container,"GLMNET",maxitglm = 10^6)
LGLMNET_CLASSIFY = classify_model(container,LGLMNET)

MAXENT = train_model(container, "MAXENT")
MAXENT_CLASSIFY = classify_model(container,MAXENT)

Y1_MAXENT = train_model(container, "MAXENT", l1_regularizer = 1, use_sgd = TRUE)
Y1_MAXENT_CLASSIFY = classify_model(container,Y1_MAXENT)

Y2_MAXENT = train_model(container, "MAXENT", l2_regularizer = 1)
Y2_MAXENT_CLASSIFY = classify_model(container,Y2_MAXENT)

Y3_MAXENT = train_model(container, "MAXENT", set_heldout = 0.2*7000)
Y3_MAXENT_CLASSIFY = classify_model(container,Y3_MAXENT)


#Total Analytics
analytics_total = create_analytics(container, 
                                     cbind(SVM_CLASSIFY, SLDA_CLASSIFY, MAXENT_CLASSIFY,GLMNET_CLASSIFY))
summary(analytics_total)

#Ensemble Analytics

#MAXENT and GLMNET
MGanalytics_total = create_analytics(container, 
                                   cbind(MAXENT_CLASSIFY,GLMNET_CLASSIFY))
summary(MGanalytics_total)

#MAXENT and SVM
MSanalytics_total = create_analytics(container, 
                                   cbind(MAXENT_CLASSIFY,SVM_CLASSIFY))
summary(MSanalytics_total)

#SVM and GLMNET
SGanalytics_total = create_analytics(container, 
                                   cbind(SVM_CLASSIFY,GLMNET_CLASSIFY))
summary(SGanalytics_total)

#Fine-Tune Analytics
Y1analytics_total = create_analytics(container, 
                                   Y1_MAXENT_CLASSIFY)
summary(Y1analytics_total)

Y2analytics_total = create_analytics(container, 
                                     Y2_MAXENT_CLASSIFY)
summary(Y2analytics_total)

Y3analytics_total = create_analytics(container, 
                                     Y3_MAXENT_CLASSIFY)
summary(Y3analytics_total)

Lanalytics_total = create_analytics(container, 
                                     LGLMNET_CLASSIFY)
summary(Lanalytics_total)

EnsembleAnal_total=create_analytics(container, 
                                    cbind(Y3_MAXENT_CLASSIFY,LGLMNET_CLASSIFY))
summary(EnsembleAnal_total)

# 6.Cross-Validation
SVM <- cross_validate(container, 4, "SVM")
SLDA <- cross_validate(container, 4, "SLDA")
MAXENT <- cross_validate(container, 4, "MAXENT")
GLMNET <- cross_validate(container, 4, "GLMNET")

# 7.Exporting Data
write.csv(analytics@document_summary, "DocumentSummary.csv")


