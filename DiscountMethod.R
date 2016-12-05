
loadGrams <- function(rDataB) { 
    unigramFile <- paste(c(newdir,'train.unigram.Rda'),collapse='')
    load(unigramFile)
    row.names(wf) <- NULL
#head(wf,100)
#changing column names
     names(wf)[names(wf) == 'Frequency'] <- 'frequency'
    names(wf)[names(wf) == 'Token'] <- 'lastTerm'

    wf <- as.data.frame(lapply(wf, unlist))
    wf <- wf[order(wf$lastTerm),]
    return(setDT(wf))
}

oneGramTable <- loadGrams('unigram')
twoGramTable <- loadGrams('bigram')
threeGramTable <- loadGrams('trigram')