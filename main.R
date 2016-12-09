rm(list=ls())
library(profr)
setwd('/Users/RDB/Library/Mobile Documents/com~apple~CloudDocs/CourseraDataScience/10_Capstone/TextPredictProject/')

#load functions that will be used 
source("functions.R")

Rprof(tmp <- tempfile())
#load raw files into memory 
source("load.R")
Rprof()
summaryRprof(tmp)

# Set the sample size percentage
# Tested different percentages: 
# 5% is relatively small after the cleaning process 
# (Rda tables:unigram + bigram+trigram + quad + pent= 8 MB)
# Could increase the percentages if only looking at one file, such as twitter by itself
# Different percentages will be kept in separate folders
percent <- 0.1
#write files to directories so you won't need to do this again
newdir <- paste(c("SwiftKey/en_US/en_US_sample_",percent * 100,"_percent/"),collapse='')
dir.create(newdir)  

#create sample files 
Rprof(tmp <- tempfile())
source("createSamples.R")
Rprof()
summaryRprof(tmp)

#if skipping creating sample then read here
#s.blogs <- readLines(con = paste(c(newdir,"s.blogs.txt"),collapse=''))
#s.twitter <- readLines(con = paste(c(newdir,"s.twitter.txt"),collapse=''))
#s.news <- readLines(con = paste(c(newdir,"s.news.txt"),collapse=''))

#clean up the sample data
#create the corpus and write corpus to file
Rprof(tmp <- tempfile())
source("clean.R")
Rprof()
summaryRprof(tmp)

#maybe a good splot for cleanup

rm(clean.corpus,s.blogs,s.news,s.twitter)
cleanData <- read.csv(file = paste(c(newdir,"clean.corpus.txt"),collapse=''))
df <- data.frame(matrix(unlist(cleanData), nrow=length(unlist(cleanData)), byrow=T),stringsAsFactors=FALSE)
rm(cleanData)
splits <- dataTrainValTest(df,'')
rm(df)

corpus <- Corpus(VectorSource(list(splits$trainset2)))
Rprof(tmp <- tempfile())
source("nGrams.R")
Rprof()
summaryRprof(tmp)

g('unigram')
g('bigram')
g('trigram')
g('quadgram')
g('pentigram')

#rm(oneGramTable,twoGramTable)
#look into unigram table, a lof of NA's beging returned
oneGramTable <- GoodTuringDiscount(loadNGrams('unigram',1))
twoGramTable <- GoodTuringDiscount(loadNGrams('bigram',2))
threeGramTable <- GoodTuringDiscount(loadNGrams('trigram',3))
fourGramTable <- GoodTuringDiscount(loadNGrams('quadgram',4))

fourGramTable[order(fourGramTable$firstTerms)]
fourGramTable_leftOverProb <- fourGramTable[, .(leftoverprob=calcLeftOverProb(lastTerm, frequency, discount)), by=firstTerms] 
threeGramTable[order(threeGramTable$firstTerms)]
threeGramTable_leftOverProb <- threeGramTable[, .(leftoverprob=calcLeftOverProb(lastTerm, frequency, discount)), by=firstTerms]
twoGramTable[order(twoGramTable$firstTerms)]
twoGramTable_leftOverProb <- twoGramTable[, .(leftoverprob=calcLeftOverProb(lastTerm, frequency, discount)), by=firstTerms]

#oneGramTable <- MLE(oneGramTable)
twoGramTable <- MLE(twoGramTable)
threeGramTable <- MLE(threeGramTable)
fourGramTable <- MLE(fourGramTable)
head(fourGramTable,100)

