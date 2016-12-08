

#perplexity - this should be applied to the cross-validation set
# the word error rate could also be created in this test

#take the maximum likelihood estimation

# p(w2 | w1) = count(w1,w2) / count (w1)
# 3-Gram example
# 'the red' count = 225
# next word is 'cross' count = 123
# 123 / 225 = 0.547

# perplexity takes the 'mle' * log base 2
# sums the numbers 
# then multiplies by 1 over the number of tests : to normalize -> L
# then it is 2 ^ L



