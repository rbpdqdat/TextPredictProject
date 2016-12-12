for (k in phrases) {
  a <- pknPredict(k)
  print(k)
  print(a)
}

pknPredict <- function(inputString) { 
#inputString <- ('I want to')
w1w2 <- getTerms2(inputString, num = 2)
w1 <- separateTerms(w1w2)$firstTerm
w2 <-separateTerms(w1w2)$lastTerm
#The original Kneser-Ney discounting (-ukndiscount) uses one discounting constant for each N-gram order. 
#These constants are estimated as
#D = n1 / (n1 + 2*n2)

#absolute discounting   
d1 <- 0.6
d2 <- 0.25
d3 <- 0.15

#Chen and Goodman's modified Kneser-Ney Discounting method
#http://www.speech.sri.com/projects/srilm/manpages/ngram-discount.7.html
#	Y   = n1/(n1+2*n2)
#	D1  = 1 - 2Y(n2/n1) #oneGramTable
#	D2  = 2 - 3Y(n3/n2) #twoGramTable
#	D3+ = 3 - 4Y(n4/n3) #threeGramTable

# get the freq of freq of n-gram to get D for smooth
#http://www.speech.sri.com/projects/srilm/manpages/ngram-discount.7.html
#  D1 <- nrow(oneGramTable[frequency==1])/
#    (nrow(oneGramTable[frequency==1])+2*nrow(oneGramTable[frequency==2]))
#  D2 <- nrow(twoGramTable[frequency==1])/
#    (nrow(twoGramTable[frequency==1])+2*nrow(twoGramTable[frequency==2]))
#  D3 <- nrow(threeGramTable[frequency==1])/
#    (nrow(threeGramTable[frequency==1])+2*nrow(threeGramTable[frequency==2]))
	
tricount <- threeGramTable[firstTerms == w1w2]
bicount <- twoGramTable[firstTerms == w2]
tw2 <- threeGramTable[grepl(paste0(w2,"$"),threeGramTable$firstTerms)]

lterm <- unique(bicount$lastTerm)
pkn <- rep(NA,length(lterm))

for (i in 1:length(lterm)) { 
pkn[i] <- max(tricount[lastTerm==lterm[i]]$frequency-d3,0)/sum(tricount$frequency) + 
  d3*(nrow(tricount)/sum(tricount$frequency)) *
  (max(grepl(paste0(w2,"_",lterm[i],'$'),threeGramTable$firstTerms)-d3,0)/nrow(tw2) +
  d3 *nrow(bicount)/sum(bicount$frequency) * 
  sum(grepl(lterm[i],twoGramTable$firstTerms))/nrow(twoGramTable))
}

predictWord <- data.frame(next_word=lterm,probability=pkn,stringsAsFactors=FALSE)
predictWord <- predictWord[order(predictWord$probability,decreasing = T),]
return(head(predictWord))
}

pknUnigram('it would mean the')

pknUnigram <- function(w) {
  w2 <- getTerms2(w, num = 1)
  predictTerms <- twoGramTable[firstTerms == w2]$lastTerm
  bigramcount <- length(predictTerms)
  pknD <- rep(NA, bigramcount)
  for (t in 1:length(predictTerms)) {
    pknD <- nrow(twoGramTable[lastTerm == predictTerms[t]]) / bigramcount
  }
  predictWord <- data.frame(next_word=predictTerms,probability=pknD,stringsAsFactors=FALSE)
  predictWord <- predictWord[order(predictWord$probability,decreasing = T),]
  return(head(predictWord))
}

#mapply(function(x,y) (max(x$freqency - d2,0))/subset(y,x$firstTerms %in% y$firstTerms), bicount,l)

getTerms2 = function(inputString, num){
  # Preprocessing
   inputString = gsub("[[:space:]]+", " ", str_trim(tolower(inputString)))
   inputString = gsub("\'+",'',inputString)
   words = unlist(strsplit(inputString, " "))
                   
   if (length(words) < num){
      stop("Number of Last Terms: Insufficient!")
   }
                   
   from = length(words)-num+1
   to = length(words)
                   
   tempWords = words[from:to]
   tempWords
   paste(tempWords, collapse="_")
}                