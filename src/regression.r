source("./src/utils.r")

runRegression <- function(formula, train.data, test.data, test.labels) {
  model.regression <<- glm(formula, data = train.data, family = binomial(link="logit"), na.action = na.pass)
  
  pred.regression.train <- plogis(predict(model.regression, newdata = train.data))
  pred.regression.train.coverted <- map(pred.regression.train, convert)
  mean(pred.regression.train.coverted == train.data$y)
  
  n <- names(train.data)
  test.data.2 <- test.data[,n[n != 'y']]
  test.data.2.levels <<- remove_missing_levels(model.regression, test.data.2)
  pred.regression.test <<- plogis(predict(model.regression, newdata = test.data.2.levels))
  test.df <- as.data.frame(rbind(pred.regression.test, test.labels))
  test.df <- test.df[sapply(test.df, function(x) !(any(is.na(x))))]
  test.df.x <- as.data.frame(map(map(test.df[1,], as.numeric), convert))
  test.df.y <- unfactor(test.df[2,])
  
  print("test data: ")
  mean(test.df.x == test.df.y)
}