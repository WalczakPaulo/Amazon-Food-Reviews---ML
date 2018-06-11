# rm(list=ls())
library(e1071)
library(rpart)
library(class) # KNN model
library(SnowballC) 
library(purrr)
library(tm)
library(readr)
set.seed(123)
source("./src/nlpProcessing.R")
source("./src/loadData.R")

reviews <- read_csv("./input/Reviews.csv")

reviews <- reviews[1:2000, ]
reviews$Score = map(reviews$Score, divideSetNumerically)

ss = unlist(reviews$Score)
# Create corpus
docs <- Corpus(VectorSource(reviews$Text))

# Clean corpus
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument, language = "english")

doc = nlpProcessing(reviews$Text)

dtm <- DocumentTermMatrix(docs)
mat.df <- as.data.frame(data.matrix(dtm), stringsAsfactors = FALSE)
mat.df <- cbind(mat.df, ss)

colnames(mat.df)[ncol(mat.df)] <- "category"

train <- sample(nrow(mat.df), ceiling(nrow(mat.df) * .80))
test <- (1:nrow(mat.df))[- train]

cl <- mat.df[, "category"]

modeldata <- mat.df[,!colnames(mat.df) %in% "category"]

knn.pred <- knn(modeldata[train, ], modeldata[test, ], cl[train], k=5)

conf.mat <- table("Predictions" = knn.pred, Actual = cl[test])
conf.mat

(accuracy <- sum(diag(conf.mat))/length(test) * 100)

df.pred <- cbind(knn.pred, modeldata[test, ])