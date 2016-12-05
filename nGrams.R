
#If you want to plot the 15 top grams
#yplot needs to be set to 1
yplot <- 0

#each nGram is set to it's type
#i.e. a unigram requires each token to have a min and a max of 1
#the 4th variable is the data set options ('train2'=training set)
#('val'= crossvalidation set) and ('test'= final test set)
#need to change the last next string to match the variable to 
#correctly save the data to a file
nGram(1,1,"unigram",corpus,"train",yplot)
nGram(2,2,"bigram",corpus,"train",yplot)
nGram(3,3,"trigram",corpus,"train",yplot)
nGram(4,4,"quadgram",corpus,"train",yplot)
nGram(5,5,"pentigram",corpus,"train",yplot)

