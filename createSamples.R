# Remove all non english characters 
# This was a major issue with reading in the data from the different sources
blogs <- iconv(blogs, "latin1", "ASCII", sub="")
news <- iconv(news, "latin1", "ASCII", sub="")
twitter <- iconv(twitter, "latin1", "ASCII", sub="")

s.blogs   <- sampleFunction(blogs, percent)
s.news   <- sampleFunction(news, percent)
s.twitter   <- sampleFunction(twitter, percent)

#remove large objects from memory  
rm(blogs,news,twitter)

write(s.blogs, paste(c(newdir,"s.blogs.txt"),collapse=''))
write(s.news, paste(c(newdir,"s.news.txt"),collapse=''))
write(s.twitter, paste(c(newdir,"s.twitter.txt"),collapse=''))


