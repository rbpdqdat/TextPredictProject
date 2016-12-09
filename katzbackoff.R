library(stringr)
library(data.table)

#"The guy in front of me just bought a pound of bacon, a bouquet, and a case of" beer pretzels cheese soda
#"You're the reason why I smile everyday. Can you follow me please? It would mean the" universe most world best
#"Hey sunshine, can you follow me and make me the" saddest bluest happiest smelliest
#"Very early observations on the Bills game: Offense still struggling but the" referees players crowd defense
#"Go on a romantic date at the" grocery mall movies beach
#"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my" way horse motorocycle phone
#"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some" thing time years weeks
#"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little" toes ears fingers eyes
#"Be grateful for the good times and keep the faith during the" hard bad worse sad
#"If this isn't the cutest thing you've ever seen, then you must be" insane asleep callous insensitive

katzPredict("and a case")
Rprof(tmp <- tempfile())
returnprob <- getProbabilityFrom3Gram("and halloween is for")


Rprof()
summaryRprof(tmp)

katzPredict = function(inputString){
  # Preprocessing
  inputString <- "a case of"
  inputTerms <- getTerms(inputString, num = 3)
  pB <- pBackOff(inputTerms,fourGramTable,NULL)
  head(pB,10)
  if (length(pB) < 1) { 
    inputTerms <- getTerms(inputString, num = 2)
    pB <- pBackOff(inputTerms,threeGramTable,fourGramTable[, .(calcLeftOverProb(lastTerm, gtAdjFreq, discount))])
  }
  
  if (length(pB) < 1) {
    inputTerms <- getTerms(inputString, num = 1)
    pB <- pBackOff(inputTerms,twoGramTable,threeGramTable[, .(calcLeftOverProb(lastTerm, gtAdjFreq, discount))])
  }
  head(threeGramTable)
  
  if (length(pB) <1) {
    inputTerms <- getTerms(inputString, num = 1)
    pB <- pBackOff(inputTerms,oneGramTable,twoGramTable[, .(calcLeftOverProb(lastTerm, gtAdjFreq, discount))])
  }
  pB
  if (length(pB) <1) {
    print('We have no shrubbery!')
  }
  
  pBackOff <- function(iTerm,gramtable,beta) {
    print(iTerm)
    if(ncol(gramtable)<4){
      matchInGram <- gramtable[lastTerm == iTerm]
    } else {
      matchInGram <- gramtable[firstTerms == iTerm]
    }
      if (nrow(matchInGram) > 0){
        lastTerms <- matchInGram$lastTerm
        all_freq <- sum(matchInGram$frequency)
        if (length(beta) < 1) { 
           matchInGram <- transform(matchInGram,prob = discount*frequency/all_freq)
        } else {
          alpha = beta / sum((matchInGram$frequency * matchInGram$discount) / all_freq)
          matchInGram <- transform(matchInGram,prob = discount*frequency/all_freq)
        }
        return(matchInGram[order(matchInGram$prob,decreasing=TRUE),])
      }
    }
}  

calcLeftOverProb <- function(lastTerm, frequency, discount){
  all_freq = sum(frequency)
  return(1-sum((discount*frequency)/all_freq))
}
##########################################################################################################
separateTerms = function(x){
  # Pre-allocate
  firstTerms = character(length(x))
  lastTerm = character(length(x))
  
  for(i in 1:length(x)){
    posOfSpaces = gregexpr("_", x[i])[[1]]
    posOfLastSpace = posOfSpaces[length(posOfSpaces)]
    firstTerms[i] = substr(x[i], 1, posOfLastSpace-1)
    lastTerm[i] = substr(x[i], posOfLastSpace+1, nchar(x[i]))
  }
  
  list(firstTerms=firstTerms, lastTerm=lastTerm)
}


##########################################################################################################
getTerms = function(inputString, num){
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

katzPredict2('and a case of')

katzPredict2 = function(inputString){
  # Preprocessing
  inputString <- 'am going to'
  matchIn4Gram <- fourGramTable[firstTerms == getTerms(inputString, num = 3)]
  if (nrow(matchIn4Gram) > 0){
    lastTerms <- matchIn4Gram$lastTerm
    all_freq <- sum(matchIn4Gram$frequency)
    matchIn4Gram$prob <- with(matchIn4Gram, (frequency * discount)/all_freq)
    #matchIn4Gram$prob <- apply(matchIn4Gram[,by = c("frequency","discount"),drop=F], 1,
    #                       function(x) {
    #                         (strtoi(x[1], base = 0L)*as.numeric(x[5])/all_freq)
    #                       })
    LeftOverProb4 <- matchIn4Gram[, .(calcLeftOverProb(lastTerm, gtAdjFreq, discount))]
  }
  #matchIn4Gram[order(matchIn4Gram$prob,decreasing=TRUE),]
  getTerms(inputString,num=2)
  matchIn3Gram <- threeGramTable[firstTerms == getTerms(inputString, num = 2)]
  if (nrow(matchIn3Gram) >0) {
    matchIn3Gram <- subset(matchIn3Gram, !(lastTerm %in% matchIn4Gram$lastTerm))
    lastTerms <- matchIn3Gram$lastTerm
    all_freq <- sum(matchIn3Gram$frequency)
    if(length(matchIn4Gram$frequency)<1) {
      matchIn3Gram$prob <- with(matchIn3Gram, (frequency * discount)/all_freq)
      #matchIn3Gram$prob <- apply(matchIn3Gram[,by = c("frequency","discount"),drop=F], 1,
      #                           function(x) {
      #                             (strtoi(x[1], base = 0L)*as.numeric(x[5])/all_freq)
      #                           })
    } else { 
       sumFreq_Disc <- sum(matchIn3Gram$frequency*matchIn3Gram$discount)
       matchIn3Gram$prob <- with(matchIn3Gram, (frequency * discount)*(LeftOverProb4/sumFreq_Disc))
       #matchIn3Gram$prob <- apply(matchIn3Gram[,by = c("frequency","discount"),drop=F], 1,
      #                        function(x) {
      #                          (strtoi(x[1], base = 0L)*as.numeric(x[5]) * 
      #                             LeftOverProb/sumFreq_Disc)
      #                        })
    } 
    LeftOverProb3 <- matchIn3Gram[, .(calcLeftOverProb(lastTerm, gtAdjFreq, discount))]
  } 
  #matchIn3Gram[order(matchIn3Gram$prob,decreasing=TRUE),]
  LeftOverProb4
  10*(.9998365/213)
  matchIn2Gram <- twoGramTable[firstTerms == getTerms(inputString, num = 1)]
  if (nrow(matchIn2Gram) >0) {
    matchIn2Gram <- subset(matchIn2Gram, !(lastTerm %in% matchIn3Gram$lastTerm))
    lastTerms <- matchIn2Gram$lastTerm
    all_freq <- sum(matchIn2Gram$frequency)
    if(length(matchIn3Gram$frequency)<1) {
      matchIn2Gram$prob <- with(matchIn2Gram, (frequency * discount)/all_freq)
      #matchIn2Gram$prob <- apply(matchIn2Gram[,by = c("frequency","discount"),drop=F], 1,
      #                           function(x) {
      #                             (strtoi(x[1], base = 0L)*as.numeric(x[5])/all_freq)
      #                           })
    } else { 
      sumFreq_Disc <- sum(matchIn2Gram$frequency*matchIn2Gram$discount)
      matchIn2Gram$prob <- with(matchIn2Gram, (frequency * discount)*(LeftOverProb3/sumFreq_Disc))
      #matchIn2Gram$prob <- apply(matchIn2Gram[,by = c("frequency","discount"),drop=F], 1,
      #                           function(x) {
      #                             (strtoi(x[1], base = 0L)*as.numeric(x[5]) * 
      #                                LeftOverProb/sumFreq_Disc)
      #                           })
    } 
    
    LeftOverProb2 <- matchIn2Gram[, .(calcLeftOverProb(lastTerm, gtAdjFreq, discount))]
  }     
  
  if (length(matchIn3Gram)<1) {   
    matchIn2Gram[order(matchIn2Gram$prob,decreasing=TRUE),]
  } else { 
    btq <- rbind(matchIn4Gram,matchIn3Gram,matchIn2Gram,fill=TRUE)
    btq
    btq[order(btq$prob,decreasing=TRUE)]
  }
}  
  
calcLeftOverProb <- function(lastTerm, frequency, discount){
  all_freq = sum(frequency)
  return(1-sum((discount*frequency)/all_freq))
}  