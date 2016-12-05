require(data.table) 
library(plyr)
library(dplyr)

dir0 ='/Users/RDB/Library/Mobile Documents/com~apple~CloudDocs/CourseraDataScience/10_Capstone/SwiftKey/en_US/en_US_sample/'
bigramFile <- paste(c(dir0,'bigram.hunspell.Rda'),collapse='')
load(bigramFile)
row.names(wf) <- NULL
#trigram.df <- head(wf)

bigram.df <- wf
#trigram.df <- trigram.df[order(trigram.df$Token),]
head(bigram.df)
system.time ({
  bigram.df$firstTerms <- lapply(as.character(bigram.df$Token), 
    function(x) {strsplit(x," ")[[1]][1]}
  )
  bigram.df$lastTerm <- lapply(as.character(bigram.df$Token), 
    function(x) {strsplit(x," ")[[1]][2]})
})

names(bigram.df)[names(bigram.df) == 'Frequency'] <- 'frequency'
head(bigram.df)
bigram.df <- as.data.frame(lapply(bigram.df, unlist))
bigram.df <- bigram.df[order(bigram.df$firstTerms),]
twoGramTable <- setDT(bigram.df)
#remove word column
twoGramTable[,Token:=NULL]
head(twoGramTable)
#typeof(head(twoGramTable$firstTerms[[1]]))
#head(twoGramTable$firstTerms)

#createtwoGramTableExtended()

##########################################################################################################
# Important assumption: This is for 3-gram table. Do similarly for 2-gram or 4-gram table to have "discount" column and leftover_probability.


# Supposed that we have data.table "twoGramTable" like this:
# 
#          firstTerms lastTerm frequency
#      1:         a_a        a        14
#      2:         a_a advisory         1
#      3:         a_a  airline         1
#      4:         a_a  alcohol         2
#      5:         a_a      all         1
#     ---                               
#8513726: zydeco_love     them         1
#8513727: zydeco_star       go         1
#8513728:   zydeco_we      got         1
#8513729:  zygote_but      how         1
#8513730:   zygote_id     only         1


##########################################################################################################
# Now we make extended 3-gram table with Discount column and Remaining Probabilities.
# Apply the formula:
# d = r* / r = ((r+1)/r)(n_(r+1)/n_r)
# For example, for frequency = 5, d = (6/5) * (N6/N5)
# N5: number of 3-grams that have frequency of 5.
# Supposed: in 3-gram, only these 3-grams appear 5 times:
#           A-B-C     5 times
#           A-B-E     5 times
#           X-Y-Z     5 times
#           L-M-N     5 times
# ==> we have N5 = 4
#createtwoGramTableExtended = function(twoGramTable){
  # Supposed table "twoGramTable" as above, we want to add a "discount" column.
  twoGramTable$discount = rep(1, nrow(twoGramTable))
  
  # Calculate the discount coefficient.
  # We only consider n-grams that have 0 < frequency <= k (5). Larger than 5: "Reliable enough".
  for (j in 5:1) {
    currRTimes = j 
    nextRTimes = currRTimes + 1
    currN = nrow(twoGramTable[frequency == currRTimes])
    nextN = nrow(twoGramTable[frequency == nextRTimes])
    currd = (nextRTimes / currRTimes) * (nextN / currN) # assumption: 0 < d < 1
    # the beauty of "data.table"!
    twoGramTable[frequency == currRTimes, discount := currd]
  }
  
  # Calculate the remaining probability (thanks to discounting...).
  twoGramTable_leftOverProb = twoGramTable[, .(leftoverprob=calcLeftOverProb(lastTerm, frequency, discount)), by=firstTerms]  
  # We now have two important objects: twoGramTable, twoGramTable_leftOverProb
  # ...

#}


##########################################################################################################
# Calculate the remaining probability (thanks to discounting...).
# Given that the input is grouped based on firstTerms already.
calcLeftOverProb = function(lastTerm, frequency, discount){
  all_freq = sum(frequency)
  return(1-sum((discount*frequency)/all_freq))
}

