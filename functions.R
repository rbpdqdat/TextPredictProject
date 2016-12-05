library(tm)
library(stringi)
library(SnowballC)
library(RWeka)
library(ggplot2)
library(R.utils)
library(hunspell)
library(caret)

loadFile <- function(fileName) { 
  bcon <- file(fileName, open='rb')
  b <- readLines(con = bcon, ok = TRUE, warn = FALSE,n=-1, encoding = "UTF-8") 
  close(bcon)
  rm(bcon,fileName)
  return(b)
}

# This is used for the raw text files and the sample files
explore.fun <- function(fileLoc,objName) { 
  c <- countLines(fileLoc)
  s <- round(file.info(fileLoc)$size / (1024*1024),2)
  ws <- strsplit(objName, "\\W+", perl = TRUE)
  w <- unlist(ws)
  wl <- length(w)
  print(paste(c("file: ",fileLoc,"; Size: ",s," MB"),collapse=''))
  print(paste(c("Number of Lines: ",c[1]),collapse=''))
  print(paste(c("Est. Word Count: ",wl, " words"),collapse=''))
  print(paste(c("Mean # Characters per word: ",round(mean(stri_length(w)),2)," characters"),collapse=''))
  print(paste(c("Max String/Line Length: ",max(nchar(objName))," characters (including spaces)"),collapse=''))
  print(paste(c("Longest Word/Character String: ",max(stri_length(w)), " characters"),collapse=''))
  rm(fileLoc,objName,c,s,w,wl,ws)
}

#Creating the sample data files
sampleFunction <- function(data, percent)
{
  return(data[as.logical(rbinom(length(data),1,percent))])
}

#This removes mispelled words from the data using the 
#huspell library
hunspellFunction <- function(sample) { 
  #sample <- readLines(sFile)
  #sample should equal the 'sample' object 
  #i.e. s.blogs,s.news,s.twitter
  #this function also omits lines with foul language
  bad_words <- hunspell(sample) 
  mod <- c()
  ct <- 1
  modct <- 1
  fcount <- 0
  cusswords <-"fuck(in)?(g)?|fuk|shit(ting)?|damn|hell|bitch(in)?(g)?|piss(ing)?|pussy|crap|fag(g)?(ot)?|slut|asshole|douche|cock"
  for (i in bad_words) {
    if (!is.null(i) & (length(i) == 0)){
      if (grepl(cusswords,sample[ct])) {
        fcount <- fcount+1
      } else { 
        mod[modct] <- sample[ct] 
        modct <- modct + 1
      }
    } 
    ct <- ct + 1
  }
  rm(sample,bad_words,ct,modct)
  return(mod)
}

#removing some periods from the data to eliminate extra line breaks
formal <- function(x) {
  formalList <- c("(Drs?)\\.+","(Mrs?)\\.+","(Phd)\\.+")
  for (j in formalList) {
    x <- gsub(j, "\\1" , x, perl=TRUE,ignore.case = 'T')
  }
  return(x)
}

#just a substitution function to keep the program clean
#pattern 1, pattern 2, orig
newLine <- function(patt1,patt2,orig) {
  orig <-gsub(patt1,patt2,orig,perl=TRUE,ignore.case='T')
  return(orig)
}

removeMostPunctuation<-
  function (x, preserve_intra_word_dashes = FALSE) 
  {
    rmpunct <- function(x) {
      x <- gsub("#", "\002", x)
      x <- gsub("[[:punct:]]+", "", x)
      gsub("\002", "#", x, fixed = TRUE)
    }
    if (preserve_intra_word_dashes) { 
      x <- gsub("(\\w)-(\\w)", "\\1\001\\2", x)
      x <- rmpunct(x)
      gsub("\001", "-", x, fixed = TRUE)
    } else {
      rmpunct(x)
    }
  }

#This will dramatically increase the number of lines while keeping the 
#number of words the same
#This should allow the n-gram size to decrease (maybe)
cleanUp <- function(a) { 
   #to reduce the number of newlines a formal function replaces 
   #periods due to title
   a <- formal(a)
   # to reduce the number of false newlines 'o.k.' is replaced with ok
   # and while replacing lines 'okay' is replaced with 'ok'
   a <- newLine("(o\\.+k\\.+)|okay", "ok",a)
   # remove words with 3 or more a's (should try this for all letters)
   # I think hunspell spell check should take care of this
   a <- newLine("aaa*","aa",a) 
   #remove single word lines
   a <- newLine("^[a-z]*$","",a)
   #converting all numbers to one number
   a <- newLine("([0-9]),", "\\1", a)
   a <- newLine("([0-9]+,?\\.*[0-9]*)", 5, a)
   #adding a space between words and numbers
   a <- newLine("([A-Z]+)(5)|(5)([A-Z]+)","\\1 \\2",a)
   #convert u.s. to unitedStates
   a <- newLine("u\\.s\\.[a\\.]*|united states[ of america]*","unitedStates",a)
   #time am/pm convert to zam or zpm 
   a <- newLine("(a)\\.m\\.|(p)\\.m\\.","z\\1\\2m",a)
   #creating a new line when a period,comma, or 'endofsentence' is found
    #...or multiple periods
   a <- newLine("\\.+|\\?+|!+|;+|,+", "\n", a)
   a[lapply(a,length)>0]
   return(unlist(lapply(a, function(x) strsplit(x, "\n")[[1]])))
}

#Creating Training Test and Validation Sets for each of the data sets
dataTrainValTest <- function(data,dName) {
  ## set the seed to make your partition reproductible
  set.seed(123)
  #if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(data)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- data[trainindex, ]
  testset <- data[-trainindex, ]
  
  traindf <- data.frame(matrix(unlist(trainset), 
                    nrow=length(unlist(trainset)), byrow=T),stringsAsFactors=FALSE)
  
  index2 <- 1:nrow(traindf)
  train2index <- sample(sample(index2, trunc(length(index2)/2)))
  trainset2 <- traindf[train2index, ]
  crossval <- traindf[-train2index, ]
  rm(trainset,index,traindf,index2,trainindex,train2index)
  list(trainset2=trainset2,testset=testset,crossval=crossval)
}

#nGram tokenizer function
nGram <- function(mn,mx,gram,corp,corpName,y) { 
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = mn, max = mx))
  dtm <- DocumentTermMatrix(corp, 
                            control = list(wordLengths=c(1, 15),tokenize = Tokenizer))
  freq <- colSums(as.matrix(dtm))
  #how many 1 word tokens are there
  uniqueTokens <- length(freq)
  #create dataframe
  wf = data.frame(Token=names(freq),Frequency = freq)
  #remove objects for memory management
  rm(freq,dtm)
  #order the dataframe
  wf <- wf[with(wf, order(-wf$Frequency)),]
  #save top 15 for plot
  wf15 <- head(wf,15)
  #remove full dataframe to control memory usage
  save(wf, file=paste(c(newdir,corpName,".",gram,".Rda"),collapse = ''))
  rm(wf)
  #set y = 1 if you want to plot
  if (y==1) { 
    p <- ggplot(wf15, aes(x=reorder(Token, -Frequency),y=Frequency))
    p <- p + geom_bar(stat="Identity", fill="red", width=0.75) 
    p <- p + geom_text(aes(label=Frequency), vjust = -0.25,size=3)
    p <- p + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
    p <- p + ggtitle("Top 15 Unigram Frequency") 
    p <- p + ylab("Frequency") 
    p <- p + xlab("Token")
    p
    rm(p,wf15)
  }
}

#loading the gram tables
loadGrams <- function(rDataB,spltNum) { 
  gramFile <- paste(c(newdir,"train.",rDataB,".Rda"),collapse='')
  load(gramFile)
  row.names(wf) <- NULL
  wf <- as.data.frame(lapply(wf, unlist))
  head(wf)
  if (spltNum > 1) { 
     if (spltNum ==2) { 
        wf$firstTerms <- lapply(as.character(wf$Token), 
                          function(x) strsplit(x," ")[[1]][1]
                          )
        wf$lastTerm <- lapply(as.character(wf$Token), 
                               function(x) strsplit(x," ")[[1]][2])
     }
     if (spltNum == 3) { 
     wf$firstTerms <- lapply(as.character(wf$Token), 
                      function(x) {paste(strsplit(x," ")[[1]][1],strsplit(x," ")[[1]][2],sep="_")}
                      )
     wf$lastTerm <- lapply(as.character(wf$Token), 
                      function(x) {strsplit(x," ")[[1]][3]})
     }
     #remove word column
     #wf[,Token:=NULL]
     wf$Token <-  NULL
  } else {
    #if unigram, the 'Token' column name is changed to 'lastTerm'
    names(wf)[names(wf) == 'Token'] <- 'lastTerm'
  }

####
  #head(wf,100)
  #changing column names
  names(wf)[names(wf) == 'Frequency'] <- 'frequency'
  wf <- as.data.frame(lapply(wf, unlist))
  wf <- wf[order(wf$lastTerm),]
  return(setDT(wf))
}

createGramTableExtended = function(GramTable){
  # Supposed table "oneGramTable" as above, we want to add a "discount" column.
  GramTable$discount = rep(1, nrow(GramTable))
  
  # Calculate the discount coefficient.
  # We only consider n-grams that have 0 < frequency <= k (5). Larger than 5: "Reliable enough".
  for (j in 5:1) {
    currRTimes = j 
    nextRTimes = currRTimes + 1
    currN = nrow(GramTable[frequency == currRTimes])
    nextN = nrow(GramTable[frequency == nextRTimes])
    currd = (nextRTimes / currRTimes) * (nextN / currN) # assumption: 0 < d < 1
    # the beauty of "data.table"!
    GramTable[frequency == currRTimes, discount := currd]
  }
  
  # Calculate the remaining probability (thanks to discounting...).
  #oneGramTable_leftOverProb = oneGramTable[, .(leftoverprob=calcLeftOverProb(lastTerm, frequency, discount)), by=firstTerms]  
  # We now have one important objects: oneGramTable, oneGramTable_leftOverProb
  # ...
  return(GramTable)
}

##########################################################################################################
# Calculate the remaining probability (thanks to discounting...).
# Given that the input is grouped based on firstTerms already.
calcLeftOverProb = function(lastTerm, frequency, discount){
  all_freq = sum(frequency)
  return(1-sum((discount*frequency)/all_freq))
}

g <- function(rDataB) { 
  rDataB <- "unigram"
  gramFile <- paste(c(newdir,"train.",rDataB,".Rda"),collapse='')
  load(gramFile)
  head(wf)
  row.names(wf) <- NULL
  write.csv(wf,paste(c(newdir,"train.",rDataB,".csv"),collapse=''),row.names = FALSE)
}
