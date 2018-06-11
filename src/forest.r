runRpart <- function(formula, train.data, test.data, test.labels) {
  model.rforest <<- rpart(formula, method="class", data=train.data, na.action = na.pass)
  
  print('Rtrain data: ')
  pred.rforest.train <<- predict(model.rforest, newdata = train.data, na.action = na.pass, type = "class")
  mean(pred.rforest.train == train.data$y)
  
  print("test data: ")
  pred.rforest.test <<- predict(model.rforest, newdata = test.data, na.action = na.pass, type = "class")
  mean(test.labels == pred.rforest.test)
}