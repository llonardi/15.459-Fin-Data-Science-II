###                                             ###
### 15.459 Financial Data Science and Computing ###
###             Youssef Berrada                 ###
###             yberrada@mit.edu                ###
###                Fall 2018                    ###

library(stringr)
library(tidyquant)
library(RTextTools)
library(tm)
library(SnowballC)
library(broom)
library(pdftools)
library(tau)
library(wordcloud)

# 1.Creating a matrix
data(USCongress)

## 1. ID - A unique identifier for the bill.
## 2. cong - The session of congress that the bill first appeared in.
## 3. billnum - The number of the bill as it appears in the congressional docket.
## 4. h_or_sen - A field specifying whether the bill was introduced in the House (HR) or the Senate (S).
## 5. major - A manually labeled topic code corresponding to the subject of the bill.


doc_matrix <- create_matrix(USCongress$text, language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms=.998)

# 2.Creating a container
container <- create_container(doc_matrix, USCongress$major, trainSize=1:4000,
                              testSize=4001:4449, virgin=FALSE)

# 3.Training models
SVM <- train_model(container,"SVM")

# 4.Classifying data using trained models
SVM_CLASSIFY <- classify_model(container, SVM)


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
