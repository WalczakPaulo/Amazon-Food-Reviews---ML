# rm(list=ls())
library(e1071)
library(rpart)
library(varhandle)

set.seed(123)
source("./src/nlpProcessing.R")
source("./src/loadData.R")
source("./src/utils.r")
source("./src/bayes.r")
source("./src/forest.r")
source("./src/regression.r")

reviews = getData()

reviews.no <- 2000
train.no <- 0.8 * reviews.no
reviews <- reviews[1:reviews.no, ]

split_val <- floor(train.no)
train_ind <- sample(seq_len(nrow(reviews)), size = split_val)
train <- reviews[train_ind, ]
test <- reviews[-train_ind, ]

reviews.text <- unlist(reviews$Text)
reviews.sc <- unlist(reviews$Score)
reviews.sc <- unlist(reviews.sc)

test.text <- unlist(test$Text)
test.sc <- unlist(test$Score)
test.sc <- unlist(test.sc)

reviews.reviews = cbind(reviews.text, reviews.sc)

reviews.corpus <- VCorpus(VectorSource(reviews.text))

# Create a document term matrix.
reviews.tdm <- DocumentTermMatrix(reviews.corpus, list(removePunctuation = TRUE,
                                                     stopwords = TRUE,
                                                     stemming = TRUE,
                                                     removeNumbers = TRUE,
                                                     tolower = TRUE))

# Convert to a data.frame for training and assign a classification (factor) to each document.
reviews.matrix <- as.matrix(reviews.tdm)
reviews.matrix <- cbind(reviews.matrix, reviews.sc)
colnames(reviews.matrix)[ncol(reviews.matrix)] <- 'y'
reviews.df <- as.data.frame(reviews.matrix)
reviews.df$y <- as.factor(reviews.df$y)
reviews.df.2 <- reviews.df[train_ind,][colSums(reviews.df[train_ind,] != 0) > 0]

train.df <- reviews.df[train_ind,]
train.df.2 <- train.df[colSums(train.df != 0) > 0]

test.df <- reviews.df[setdiff(1:reviews.no, train_ind),]
test.df.y <- subset(test.df, select = c(y))
test.df <- subset(test.df, select = -c(y))
test.df.2 <- test.df[colSums(test.df != 0) > 0]
test.sc <- map(test.df.y, as.character)$y

# Train.
formula <- y ~ .
runBayes(formula, train.df, test.df, test.sc)
runRpart(formula, reviews.df, train.df, train_ind, test.df, test.sc)
runRegression(formula, train.df.2, test.df, test.sc)
