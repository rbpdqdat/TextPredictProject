########## Kneser-Ney prepping data sets ##########

KNprep <- function(dt, n) {
  # Takes a data.table coming from ngramprep() and adds (Mod.) Kneser-Ney-discounting of the freqs
  #
  # Args: 
  #    dt: a data.table as ngramprep() serves it, requires Terms, Nextword and Freq as minimum cols
  #    n:  the ngram level (one dt per ngram level 1-4) 
  # Returns:
  #    dt: The prepared data table now also suitable for Modified Kneser-Ney prediction
  #        as described by Chen & Goodman (1999)
  
  col <- colnames(dt)
  col <- col[1:n]
  
  
  # Y = N_c / (N_c + 2N_(c+1)), where N_c is the count of ngrams with count==c (for Kneser-Ney)
  #     Note: This is a simplified calculation of Y using only the two lowest kept freqs.
  c1 <- min(dt$Freq) # The lowest available Freq
  c2 <- min(subset(dt, Freq>c1)$Freq) # And the second lowest
  Y <- nrow(dt[Freq == c1]) / (nrow(dt[Freq == c1]) + 2 * nrow(dt[Freq == c2])) # 1:0.50 2:0.56 3+:0.62  
  
  # D = Discounting parameter different for freq==1, freq==2 and freq>=3
  #     Ref Goodman and Chen (1999)
  dt[, D := 0]
  dt[Freq == 1]$D <- 1 - 2 * Y * (nrow(dt[Freq == 2]) / nrow(dt[Freq == 1]))
  dt[Freq == 2]$D <- 2 - 3 * Y * (nrow(dt[Freq == 3]) / nrow(dt[Freq == 2]))
  dt[Freq > 2]$D  <- 3 - 4 * Y * (nrow(dt[Freq == 4]) / nrow(dt[Freq == 3]))
  
  # Nom = First nominator in P_KN formula ( max{c(w_i-1, w_i)-D, 0} )
  dt <- dt[, Nom := pmax(Freq-D, 0)]
  
  # Denom = Denominator is the count of the preceding word(s) 
  if(n==1) {
    dt <- dt[, Denom := sum(Freq)]
  } else if (n==2) {
    dt <- dt[, .(w, Freq, D, Nom, Denom = sum(Freq)), by = w1]
  } else if (n==3) {
    dt <- dt[, .(w, Freq, D, Nom, Denom = sum(Freq)), by = list(w2, w1)]
  } else if (n==4) {
    dt <- dt[, .(w, Freq, D, Nom, Denom = sum(Freq)), by = list(w3, w2, w1)]
  }
  
  
  # NN = number of word types that follows w_i-1 in the training data
  if(n==1) {
    dt <- dt[, .(w, Freq, D, Nom, Denom, NN = length(w))]
  } else if (n==2) {
    dt <- dt[, .(w, Freq, D, Nom, Denom, NN = length(w)), by=w1]
  } else if (n==3) {
    dt <- dt[, .(w, Freq, D, Nom, Denom, NN = length(w)), by=list(w2, w1)]
  } else if (n==4) {
    dt <- dt[, .(w, Freq, D, Nom, Denom, NN = length(w)), by=list(w3, w2, w1)]
  }
  
  
  # L  = Lambda, normalizing constant, the probability mass we've discounted
  dt[, L := (D / Freq) * NN]
  
  # N = The number of different ngrams this nextword completes in training set 
  #     (c(w_(i-1)) for Kneser-Ney). Used in P_continutation (PC)
  if(n==1) {
    dt <- dt[, .(w, Freq, D, Nom, Denom, NN, L, .N)]
  } else if (n==2) {
    dt <- dt[, .(w1, Freq, D, Nom, Denom, NN, L, .N), by=w]
  } else if (n==3) {
    dt <- dt[, .(w2, w1, Freq, D, Nom, Denom, NN, L, .N), by=w]
  } else if (n==4) {
    dt <- dt[, .(w3, w2, w1, Freq, D, Nom, Denom, NN, L, .N), by=w]
  }
  
  
  # PC = P_continuation
  dt[, PC := N / nrow(dt)] # Count of this novel continuation div. by number of unique grams
  
  # Prob_KN - Estimated KN probability
  dt[, P_KN := (Nom/Denom) + ((D/Denom) * NN) * PC]
  .
  .
  .