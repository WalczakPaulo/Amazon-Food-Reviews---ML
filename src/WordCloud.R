library(RSQLite)
source("./src/makeWordCloud.R")

setwd("D:/Users/Paul/mow")
db <- dbConnect(dbDriver("SQLite"), "./input/database.sqlite")

reviews <- dbGetQuery(db, "
                      SELECT *
                      FROM Reviews
                      LIMIT 1000")

png("wordcloud.png")
make_word_cloud(reviews$Text, TRUE)
dev.off()

#Word clouds by score
png("wordcloud1.png")
make_word_cloud(reviews$Text[reviews$Score == 1], FALSE)
dev.off()

png("wordcloud2.png")
make_word_cloud(reviews$Text[reviews$Score == 2], F)
dev.off()

png("wordcloud3.png")
make_word_cloud(reviews$Text[reviews$Score == 3], F)
dev.off()

png("wordcloud4.png")
make_word_cloud(reviews$Text[reviews$Score == 4], F)
dev.off()

png("wordcloud5.png")
make_word_cloud(reviews$Text[reviews$Score == 5], F)
dev.off()

png("wordcloudPos.png")
make_word_cloud(reviews$Text[reviews$Score > 3], F)
dev.off()

png("wordcloudNeg.png")
make_word_cloud(reviews$Text[reviews$Score < 3 ], F)
dev.off()


dbDisconnect(db)
remove(list = ls())
gc()