setwd("D:/Users/Paul/mowy")
source("./src/nlpProcessing.R")
source("./src/loadData.R")

reviews = getData()

split_val <- floor(0.8 * nrow(reviews))
train_ind <- sample(seq_len(nrow(reviews)), size = split_val)
train <- reviews[train_ind, ]
test <- reviews[-train_ind, ]


tt = nlpProcessing(test$Text)
df <- data.frame(text = get("content", tt))
dataframe<-data.frame(text=unlist(sapply(tt, `[`)), stringsAsFactors=F)


