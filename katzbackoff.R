library(stringr)
library(data.table)

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
 phrases <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", 
 "You're the reason why I smile everyday. Can you follow me please? It would mean the", 
 "Hey sunshine, can you follow me and make me the", 
 "Very early observations on the Bills game: Offense still struggling but the", 
 "Go on a romantic date at the", 
 "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", 
 "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", 
 "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", 
 "Be grateful for the good times and keep the faith during the", 
 "If this isn't the cutest thing you've ever seen, then you must be",
 "something to understand", "a report like this")

#beer pretzels cheese soda
#universe most world best
#saddest bluest happiest smelliest
#referees players crowd defense
#grocery mall movies beach
#way horse motorocycle phone
#thing time years weeks
#toes ears fingers eyes
#hard bad worse sad
#insane asleep callous insensitive

for (k in phrases) {
  a <- katzPredict2(k)
  print(head(a))
}

katzPredict2 = function(inputString){
  #inputString <- 'and a case of'
  #4Gram

  match4Gram <- fourGramTable[firstTerms == getTerms(inputString, num = 3)]
  if (nrow(match4Gram) > 0) {
    all_freq <- sum(match4Gram$frequency)
    match4Gram$prob <- with(match4Gram, (frequency * discount)/all_freq)
    beta_prob4 <- fourGramTable_leftOverProb[firstTerms == match4Gram$firstTerms[1]]$leftoverprob
    btq <- match4Gram
    #The probability of the 3 word match is increased by 100
    #because lower order matches are superceding the low probability matches
    btq$prob <- btq$prob*100
  }

  #3Gram
  match3Gram <- threeGramTable[firstTerms == getTerms(inputString, num = 2)]
  if (nrow(match3Gram) >0) {
    #if there are matches in 4Gram then start Leftover probability process
    #will determine if the probability of the next word calculated is above a 
    #certain level to cut off the process
    if (nrow(match4Gram) > 0) { 
      match3Gram <- subset(match3Gram, !(lastTerm %in% match4Gram$lastTerm))
      if(nrow(match3Gram)>0) { 
        all_freq <- sum(match3Gram$frequency)
        alpha <- beta_prob4 * sum(match3Gram$frequency*match3Gram$discount)/ all_freq
        match3Gram$prob <- unlist(mapply(function(x,y) alpha*((y*x)/all_freq), 
                                match3Gram$discount,match3Gram$frequency))
        btq <- rbind(btq,match3Gram,fill=TRUE)
      }
    } else { 
      all_freq <- sum(match3Gram$frequency)
      match3Gram$prob <- with(match3Gram, (frequency * discount)/all_freq)
      btq <- match3Gram
    }      
    beta_prob3 <- threeGramTable_leftOverProb[firstTerms == match3Gram$firstTerms[1]]$leftoverprob
    } 

  #2Gram
  match2Gram <- twoGramTable[firstTerms == getTerms(inputString, num = 1)]
  if (nrow(match2Gram) >0) {
    if (nrow(match3Gram) > 0) { 
      match2Gram <- subset(match2Gram, !(lastTerm %in% match3Gram$lastTerm))
      if (nrow(match2Gram) >0) { 
        all_freq <- sum(match2Gram$frequency)
        alpha <- beta_prob3 * sum(match2Gram$frequency*match2Gram$discount)/ all_freq
        match2Gram$prob <- unlist(mapply(function(x,y) alpha*((y*x)/all_freq), 
                                         match2Gram$discount,match2Gram$frequency))
        btq <- rbind(btq,match2Gram,fill=TRUE)
      }
    } else { 
      all_freq <- sum(match2Gram$frequency)
      match2Gram$prob <- with(match2Gram, (frequency * discount)/all_freq)
      btq <- match2Gram[prob > 0.001]
    }
    beta_prob2 <- twoGramTable_leftOverProb[firstTerms == match2Gram$firstTerms[1]]$leftoverprob
  } 

  #1Gram
  #match1Gram <- oneGramTable[lastTerm == getTerms(inputString, num = 1)]
  #only match the 1Gram when there are no other matches
  #because the probability is skewed and there is no predictive power based on previous
  # words
  #therefore only the highest frequency words are used
  match1Gram <- head(oneGramTable[order(oneGramTable$frequency,decreasing=TRUE)],10)
    if (nrow(match2Gram) < 1) { 
      all_freq <- sum(match1Gram$frequency)
      match1Gram$prob <- with(match1Gram, (frequency * discount)/all_freq)
      btq <- match1Gram
    }
  if (nrow(btq) > 0) { 
     return(btq[order(btq$prob,decreasing=TRUE)])
     #return(sum(btq$prob))
    } else {
     return('No matches found')
  }
}

calcLOverProb <- function(lastTerm, frequency, discount){
  all_freq = sum(frequency)
  return(1-sum((discount*frequency)/all_freq))
}  