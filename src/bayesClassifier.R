library(readr)
library(caret)
library(tm)
library(purrr)

setwd("D:/Users/Paul/mowy")

divideSet <- function(data) {
  result <- ""
  if(data < 3)
    result <- "negative"
  else 
    result <- "positive"
  return(result)
}

reviews <- read_csv("./input/Reviews.csv")
reviews <- reviews[1:25, ]
text <- unlist(reviews$Text)
sc <- unlist(reviews$Score)
sc = map(sc, divideSet)
sc <- unlist(sc)


reviews = cbind(text,sc)



data <- text
corpus <- VCorpus(VectorSource(data))

# Create a document term matrix.
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))

# Convert to a data.frame for training and assign a classification (factor) to each document.
train <- as.matrix(tdm)
train <- cbind(train, sc)
colnames(train)[ncol(train)] <- 'y'
train <- as.data.frame(train)
train$y <- as.factor(train$y)

# Train.
fit <- train(y ~ ., data = train, method = 'bayesglm')

# Check accuracy on training.
predict(fit, newdata = train)

# Test data.
