runBayes <- function(formula, train.data, test.data, test.labels, laplace = 0) {
  model.bayes <<- naiveBayes(formula, data=train.data, laplace = laplace)
  pred.bayes.train <<- predict(model.bayes, newdata = train.data, type = "class")
  pred.bayes.test <<- predict(model.bayes, newdata = test.data, type = "class")
  
  print("train data")
  mean(train.data$y == pred.bayes.train)
  print("test data")
  mean(test.labels == pred.bayes.test)
}