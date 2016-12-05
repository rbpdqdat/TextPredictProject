#First a spell check is performed on each set
#The 'mispelled' words are removed from the entire samples
#A new file is saved as a checkpoint
s.blogs <- hunspellFunction(s.blogs)
s.blogs <- cleanUp(s.blogs)
write(s.blogs, paste(c(newdir,"hunspell.s.blogs.txt"),collapse=''))

s.news <- hunspellFunction(s.news)
s.news <- cleanUp(s.news)
write(s.news, paste(c(newdir,"hunspell.s.news.txt"),collapse=''))

s.twitter <- hunspellFunction(s.twitter)
s.twitter <- cleanUp(s.twitter)
write(s.twitter, paste(c(newdir,"hunspell.s.twitter.txt"),collapse=''))

#a corpus is created with all of the files
#s.corpus <- c(readLines("s.hunspell.blogs.txt"),readLines("s.hunspell.news.txt"),readLines("s.hunspell.twitter.txt"))
#rememeber is one or more files are not used, 
#just remove it from the creation of 's.corpus'
s.blogs <- readLines(con = paste(c(newdir,"hunspell.s.blogs.txt"),collapse=''))
s.twitter <- readLines(con = paste(c(newdir,"hunspell.s.twitter.txt"),collapse=''))
s.news <- readLines(con = paste(c(newdir,"hunspell.s.news.txt"),collapse=''))

s.corpus <- c(s.blogs,s.news,s.twitter)
cleaning.scorpus <- Corpus(VectorSource(list(s.corpus)))
#cleaning up memory usage
rm(s.corpus)

cleaning.scorpus <- tm_map(cleaning.scorpus, content_transformer(tolower))
cleaning.scorpus <- tm_map(cleaning.scorpus, content_transformer(removeMostPunctuation),
                      preserve_intra_word_dashes = TRUE)
#cleaning.scorpus <- tm_map(cleaning.scorpus, removePunctuation,preserve_intra_word_dashes = TRUE)
#cleaning.scorpus <- tm_map(cleaning.scorpus, removeNumbers)
#cleaning.scorpus <- tm_map(cleaning.scorpus, removeWords, stopwords("english"))
clean.corpus <- tm_map(cleaning.scorpus, stripWhitespace)
rm(cleaning.scorpus)
#cleaning.scorpus
#summary(cleaning.scorpus)
#write the corpus to a txt file
writeCorpus(clean.corpus, filenames=paste(c(newdir,"clean.corpus.txt"),collapse=''))
