library(RSQLite)
library(purrr)
setwd("D:/Users/Paul/mow")
source("./src/makeWordCloud.R")


getData <- function() {
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
  dbDisconnect(db)
  #return(list("train"=train,"test"=test))
  return(reviews)
}

