rm(list=ls())
library(e1071)
library(rpart)
setwd("D:/Users/Paul/mowy")
source("./src/nlpProcessing.R")
source("./src/loadData.R")

reviews = getData()

reviews <- reviews[1:500, ]

split_val <- floor(0.8 * nrow(reviews))
train_ind <- sample(seq_len(nrow(reviews)), size = split_val)
train <- reviews[train_ind, ]
test <- reviews[-train_ind, ]

text <- unlist(train$Text)
sc <- unlist(train$Score)
sc <- unlist(sc)

textTestSet <- unlist(test$Text)
scTest <- unlist(test$Score)
scTest <- unlist(scTest)

reviewsTest = cbind(textTestSet, scTest)

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
#fit <- train(y ~ ., data = train, method = 'bayesglm', verboseiter = TRUE)
model <- naiveBayes(y ~ ., data=train)
fit <- rpart(y ~ ., method="class", data=train, na.action = "pass")

print('Display accuracies of prediction on train set')
print('Naive bayes:')
mean(train$y == predict(model, newdata = train))
print('Regression decision tree:')
val = predict(fit, newdata = train,
              na.action = na.pass)
sc = val[,1] - val[,2]
sc = map(sc, divide)
mean(sc ==  train$y)
# Test data.
# Test data.
data2 <- textTestSet
corpus <- VCorpus(VectorSource(data2))
tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(tdm), removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
test <- as.matrix(tdm)

print('Check accuracy on test.')
print('Naive bayes: ')
mean(scTest == predict(model, newdata = test))
val = predict(fit, newdata = test,
              na.action = na.pass)
sc = val[,1] - val[,2]
sc = map(sc, divide)
mean(sc ==  scTest)
