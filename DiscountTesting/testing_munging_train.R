library(ggplot2)
library(scales)
rm(list=ls())

loadGrams <- function(i,j) {
  newDir <- paste(c("SwiftKey/en_US/en_US_sample_",i,"_percent/"),collapse='')
  gramFile <- paste(c(newDir,"train.",j,".Rda"),collapse='')
  load(gramFile)
  return(wf$Frequency)
}

na.pad <- function(x,len){
  x[1:len]
}

makePaddedDataFrame <- function(l,...){
  maxlen <- max(sapply(l,length))
  data.frame(lapply(l,na.pad,len=maxlen),...)
}

percent <- c(1,5,10)
ngram <- c("unigram","bigram","trigram","quadgram","pentigram")

for (k in percent) { 
  for (u in ngram) { 
   # un <- paste(c(u,k),collapse='')
    n <- loadGrams(k,u)
   # assign(un, n)
    assign(u, n)
  }
  d <- paste(c("df",k),collapse = '')
  assign(d, makePaddedDataFrame(list(unigram=unigram,
                                     bigram=bigram,trigram=trigram,
                                     quadgram=quadgram,pentigram=pentigram
  )))
}

nGramPlot(df1,1)

ngramTot <- function(gram) sum(gram,na.rm=TRUE)
cols <- c("unigram" = "red","bigram" = "blue","trigram" = "green",
          "quadgram"= "orange","pentigram"="purple")


nGramPlot <- function(df,pcnt) {
     p <- ggplot() 
     p <- p + geom_line(data=df, aes(x = seq(1, length(df$unigram)), 
                  y = (cumsum(df$unigram)/ngramTot(df$unigram)) * 100,colour="unigram"))
     p <- p + geom_line(data=df, aes(x = seq(1, length(df$bigram)), 
                                y = (cumsum(df$bigram)/ngramTot(df$bigram)) * 100,colour="bigram"))
     p <- p + geom_line(data=df, aes(x = seq(1, length(df$trigram)), 
                                     y = (cumsum(df$trigram)/ngramTot(df$trigram)) * 100,colour="trigram"))
     p <- p + geom_line(data=df, aes(x = seq(1, length(df$quadgram)), 
                                     y = (cumsum(df$quadgram)/ngramTot(df$quadgram)) * 100,colour="quadgram"))
     p <- p + geom_line(data=df, aes(x = seq(1, length(df$pentigram)), 
                                     y = (cumsum(df$pentigram)/ngramTot(df$pentigram)) * 100,colour="pentigram"))
     p <- p + ylab("Percentage of total n-grams") 
     p <- p + xlab(paste(c("# Unique n-Grams in ",pcnt,"% File"),collapse = ''))
     p <- p + scale_colour_manual(name="n-grams",values = cols, 
                                  breaks = c("unigram","bigram","trigram",
                                             "quadgram","pentigram"))
     p <- p + scale_x_continuous(labels = comma)
     p
}
