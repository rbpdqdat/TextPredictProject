#reading all files to be used to create a full corpus
#reading in could take over a minute as the files sizes are relatively large
#comment out any files that are not intended to be loaded

blogs <- loadFile('SwiftKey/en_US/en_US.blogs.txt')
news <- loadFile('SwiftKey/en_US/en_US.news.txt')
twitter <- loadFile('SwiftKey/en_US/en_US.twitter.txt')






