rm(list=ls())
library(e1071)
library(rpart)
library(class) # KNN model
library(SnowballC) 
library(purrr)
library(tm)
library(readr)

source("./src/nlpProcessing.R")
source("./src/loadData.R")


reviews <- read_csv("./input/Reviews.csv")


reviews <- reviews[1:1000, ]
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

# Create dtm
dtm <- DocumentTermMatrix(docs)

# Transform dtm to matrix to data frame - df is easier to work with
mat.df <- as.data.frame(data.matrix(dtm), stringsAsfactors = FALSE)

# Column bind category (known classification)
mat.df <- cbind(mat.df, ss)

# Change name of new column to "category"
colnames(mat.df)[ncol(mat.df)] <- "category"

# Split data by rownumber into two equal portions
train <- sample(nrow(mat.df), ceiling(nrow(mat.df) * .80))
test <- (1:nrow(mat.df))[- train]

# Isolate classifier
cl <- mat.df[, "category"]

# Create model data and remove "category"
modeldata <- mat.df[,!colnames(mat.df) %in% "category"]

# Create model: training set, test set, training set classifier
knn.pred <- knn(modeldata[train, ], modeldata[test, ], cl[train], k=5)

# Confusion matrix
conf.mat <- table("Predictions" = knn.pred, Actual = cl[test])
conf.mat

# Accuracy
(accuracy <- sum(diag(conf.mat))/length(test) * 100)

# Create data frame with test data and predicted category
df.pred <- cbind(knn.pred, modeldata[test, ])
#write.table(df.pred, file="output.csv", sep=";")