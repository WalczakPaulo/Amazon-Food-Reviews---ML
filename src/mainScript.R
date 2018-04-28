rm(list=ls())
library(e1071)
setwd("D:/Users/Paul/mowy")
source("./src/nlpProcessing.R")
source("./src/loadData.R")

reviews = getData()


split_val <- floor(0.8 * nrow(reviews))
train_ind <- sample(seq_len(nrow(reviews)), size = split_val)
train <- reviews[train_ind, ]
test <- reviews[-train_ind, ]



tt = nlpProcessing(train$Text)
frequencies = DocumentTermMatrix(tt)
fq <- as.data.frame(as.matrix(frequencies))
fq$Score = unlist(train$Score)
fq$Score = as.factor(fq$Score)
#colnames(fq) = NULL

ts = nlpProcessing(test$Text)
frequenciest = DocumentTermMatrix(ts)
fqt <- as.data.frame(as.matrix(frequenciest))
fqt$Score = unlist(test$Score)
fqt$Score = as.factor(fqt$Score)

model <- naiveBayes(fq$Score ~ ., data=fq)
predict(model, fqt)
