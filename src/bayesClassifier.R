library(readr)
library(caret)
library(tm)
library(purrr)
library(e1071)
library(rpart)


divideSet <- function(data) {
  result <- ""
  if(data < 3)
    result <- "negative"
  else 
    result <- "positive"
  return(result)
}

reviews <- read_csv("./input/Reviews.csv")
reviews <- reviews[1:1000, ]

split_val <- floor(0.8 * nrow(reviews))
train_ind <- sample(seq_len(nrow(reviews)), size = split_val)
train <- reviews[train_ind, ]
test <- reviews[-train_ind, ]

text <- unlist(train$Text)
sc <- unlist(train$Score)
sc = map(sc, divideSet)
sc <- unlist(sc)

textTestSet <- unlist(test$Text)
scTest <- unlist(test$Score)
scTest = map(scTest, divideSet)
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
#mean(fqt$Score == predict(model, fqt))
# Check accuracy on training.
#mean(train$y == predict(model, newdata = train))

# Test data.
# Test data.
data2 <- textTestSet
corpus <- VCorpus(VectorSource(data2))
tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(tdm), removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
test <- as.matrix(tdm)

# Check accuracy on test.
#mean(scTest == predict(model, newdata = test))

#modelRegression <- glm(y ~ ., data=train, family="binomial")
#mean(fqt$Score == predict(modelRegression, fqt,type="response"))

#mean(scTest == predict(modelRegression, newdata = as.data.frame(test)))


fit <- rpart(y ~ ., method="class", data=train)
val = predict(fit, newdata = train,
        na.action = na.pass)

divide <- function(item) {
  result = ""
  if(item < 0) 
    result = "positive"
  else 
    result = "negative"
  return(result)
}

sc = val[,1] - val[,2]
sc = map(sc, divide)

mean(sc == train$y)