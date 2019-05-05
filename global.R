
library(stringr)


wines_df <- read.csv('data/wine_revs.csv', stringsAsFactors = F)

common_words <- unlist(str_split(readLines("data/common_words_and_bigrams.txt"), pattern = ","))






