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
percent <- 0.05
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
#I need to understand how this is organizing the data
#the R 'lists' are driving me crazy
#str(splits)
#lapply(splits,nrow)
#lapply(splits,head)
#splits$trainset2
#Rprof()
#summaryRprof(tmp)
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
oneGramTable <- loadGrams('unigram',1)
twoGramTable <- loadGrams('bigram',2)
threeGramTable <- loadGrams('trigram',3)

oneGramTable <- createGramTableExtended(oneGramTable)
head(oneGramTable)
tail(oneGramTable)
object.size(oneGramTable) / 1024^2
twoGramTable <- createGramTableExtended(twoGramTable)
head(twoGramTable)
tail(twoGramTable)
object.size(twoGramTable) / 1024^2
threeGramTable <- createGramTableExtended(threeGramTable)
head(threeGramTable)
tail(threeGramTable)
object.size(threeGramTable) / 1024^2

