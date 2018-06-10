rm(list=ls())
library(e1071)
library(rpart)

set.seed(123)
source("./src/nlpProcessing.R")
source("./src/loadData.R")

reviews = getData()

reviews <- reviews[1:1000, ]

split_val <- floor(0.8 * nrow(reviews))
train_ind <- sample(seq_len(nrow(reviews)), size = split_val)
train <- reviews[train_ind, ]
test <- reviews[-train_ind, ]

reviews.text <- unlist(reviews$Text)
reviews.sc <- unlist(reviews$Score)
reviews.sc <- unlist(reviews.sc)

# train.text <- unlist(train$Text)
# train.sc <- unlist(train$Score)
# train.sc <- unlist(train.sc)

test.text <- unlist(test$Text)
test.sc <- unlist(test$Score)
test.sc <- unlist(test.sc)

reviews.reviews = cbind(reviews.text, reviews.sc)
# train.reviews = cbind(train.text, train.sc)
# test.reviews = cbind(test.text, test.sc)

reviews.corpus <- VCorpus(VectorSource(reviews.text))
# train.corpus <- VCorpus(VectorSource(train.text))
# test.corpus <- VCorpus(VectorSource(test.text))

# Create a document term matrix.
reviews.tdm <- DocumentTermMatrix(reviews.corpus, list(removePunctuation = TRUE,
                                                     stopwords = TRUE,
                                                     stemming = TRUE,
                                                     removeNumbers = TRUE,
                                                     tolower = TRUE))
# train.tdm <- DocumentTermMatrix(train.corpus, list(dictionary = Terms(reviews.tdm),
#                                                    removePunctuation = TRUE,
#                                                    stopwords = TRUE,
#                                                    stemming = TRUE,
#                                                    removeNumbers = TRUE,
#                                                    tolower = TRUE))
# test.tdm <- DocumentTermMatrix(test.corpus, list(dictionary = Terms(train.tdm),
#                                                  removePunctuation = TRUE,
#                                                  stopwords = TRUE,
#                                                  stemming = TRUE,
#                                                  removeNumbers = TRUE,
#                                                  tolower = TRUE))

# Convert to a data.frame for training and assign a classification (factor) to each document.
reviews.matrix <- as.matrix(reviews.tdm)
reviews.matrix <- cbind(reviews.matrix, reviews.sc)
colnames(reviews.matrix)[ncol(reviews.matrix)] <- 'y'
reviews.df <- as.data.frame(reviews.matrix)
reviews.df$y <- as.factor(reviews.df$y)

train.df <- reviews.df[train_ind,]
test.df <- reviews.df[setdiff(1:200, train_ind),]
# train.matrix <- as.matrix(train.tdm)
# train.matrix <- cbind(train.matrix, train.sc)
# colnames(train.matrix)[ncol(train.matrix)] <- 'y'
# train.df <- as.data.frame(train.matrix)
# train.df$y <- as.factor(train.df$y)
# 
# test.matrix <- as.matrix(test.tdm)
# test.df <- as.data.frame(test.matrix)
# test.df.trimmed <- test.df[colSums(test.df != 0) > 0]

# Train.
#fit <- train(y ~ ., data = train, method = 'bayesglm', verboseiter = TRUE)
formula <- y ~ .
model.bayes <- naiveBayes(formula, data=train.df)
model.rforest <- rpart(formula, method="class", data=reviews.df, subset = train_ind, na.action = na.pass)
model.regression <- lm(formula, data = reviews.df, subset = train_ind, family = binomial(link="logit"), na.action = na.pass)

print('Display accuracies of prediction on train set')
print('Naive bayes:')
mean(train.df$y == predict(model.bayes, newdata = train.df))
print('Regression decision tree:')
val = predict(model.rforest, newdata = train.df,
              na.action = na.pass)
sc = val[,1] - val[,2]
sc = map(sc, divide)
mean(sc == train.df$y)

#Test set
print('Check accuracy on test.')
print('Naive bayes: ')
mean(test.sc == predict(model.bayes, newdata = test.df))
val = predict(model.rforest, newdata = test.df, na.action = na.pass, type = "class")
mean(test.sc == val)
