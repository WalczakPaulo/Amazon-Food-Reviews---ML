library(RSQLite)
library(purrr)
setwd("D:/Users/Paul/mow")
source("./src/makeWordCloud.R")

setwd("D:/Users/Paul/mow")
db <- dbConnect(dbDriver("SQLite"), "./input/database.sqlite")

reviews <- dbGetQuery(db, "
                      SELECT Score,Text
                      FROM Reviews WHERE Score != 3
                      LIMIT 10")

divideSet <- function(data) {
  result <- ""
  if(data < 3)
      result <- "negative"
  else 
      result <- "positive"
  return(result)
}

reviews$Score = map(reviews$Score, divideSet)

split_val <- floor(0.8 * nrow(reviews))

## set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(reviews)), size = split_val)

train <- reviews[train_ind, ]
test <- reviews[-train_ind, ]

dbDisconnect(db)

