##Code for homework 1 Question 3
library(quanteda)
library(quantedaData)


removeChaff <- function(text) {
  headerEndRegexp = "\\*\\*\\* START OF TH.*$"
  
  headerPos = regexpr(headerEndRegexp, text)
  # The mystery doc doesn't have a header
  if (headerPos == -1) {
    print("no header, mystery doc?")
    headerPos = attr(headerPos, 'match.length')
  }
  
  footerBeginRegexp = "End of the Project Gutenberg EBook of.*$"
  footerPos = regexpr(footerBeginRegexp, text)
  if (footerPos == -1) {
    footerBeginRegexp = "\\*\\*\\*END OF THE PROJECT GUTENBERG EBOOK.*$"
    footerPos = regexpr(footerBeginRegexp, text)
  }
  # All documents have the footer
  stopifnot(footerPos != -1)
  
  return(substr(text, headerPos, footerPos))
}

#last_speech_text<-inaugCorpus$document$texts[57]
createDfm <- function(corpus, funcWordDict) {
  allBlocks = character(0)
  authorsByBlock = character(0)
  kBlockSize = 1700
  for (i in 1:length(corpus$documents$texts)) {
    text = corpus$documents$texts[[i]]
    text = removeChaff(text)
    stopifnot(length(text) > 0)
    tokens <- tokenize(toLower(text), simplify=TRUE, removePunct=TRUE)
    #selectFeatures(tokens, funcWordDict)  # keep only function words
    # Partition the tokens into blocks
    partitionIndices = seq(1, length(tokens), kBlockSize)
    blocks = sapply(partitionIndices, 
                    function(i) tokens[i:min(i+kBlockSize-1, length(tokens))])    
    # rejoining all the tokens together, because I can't figure out how to make 
    # the dfm constructor take a list of lists as input.
    author = docvars(textCorpus, 'authors')[[i]]
    for (j in 1:length(blocks)) {
      allBlocks <- c(allBlocks, paste(blocks[j], collapse=" "))
      authorsByBlock <- c(authorsByBlock, author)
    }
  }
  blockDfm = dfm(allBlocks)
  blockDfm = selectFeatures(blockDfm, funcWordDict)  # keep only function words
  return(list(blockDfm, authorsByBlock))
}


# NOTE: I had to rename 'mystery.txt' to be 'mystery_mystery.txt' for this to work.
textCorpus <- corpus(textfile("~/Text_as_Data/dickens_austen/*.txt", 
                              docvarsfrom = c("filenames"), dvsep = "_", 
                              docvarnames=c("authors", "titles")))
austenCorpus = subset(textCorpus, authors == 'austen')
dickensCorpus = subset(textCorpus, authors == 'dickens')
mysteryCorpus = subset(textCorpus, authors == 'mystery')

functionWords = c('a', 'been', 'had', 'its', 'one', 'the', 'were', 'all', 'but', 'has', 'may',
                  'only', 'their', 'what', 'also', 'by', 'have', 'more', 'or', 'then', 'when',
                  'an', 'can', 'her', 'must', 'our', 'there', 'which', 'and', 'do', 'his', 'my',
                  'should', 'things', 'who', 'any', 'down', 'if', 'no', 'so', 'this', 'will',
                  'are', 'even', 'in', 'not', 'some', 'to', 'with', 'as', 'every', 'into', 'now',
                  'such', 'up', 'would', 'at', 'for', 'is', 'of', 'than', 'upon', 'your', 'be', 
                  'from', 'it', 'on', 'that', 'was')
funcWordDict = dictionary(list(func = functionWords))

#snippets_dfm = createDfm(textCorpus, funcWordDict)
fullTextTuple = createDfm(textCorpus, funcWordDict)
snippets_dfm = fullTextTuple[[1]]
authors = fullTextTuple[[2]]
austen_snippets = createDfm(austenCorpus, funcWordDict)[[1]]
dickens_snippets = createDfm(dickensCorpus, funcWordDict)[[1]]
mystery_dfm = createDfm(mysteryCorpus, funcWordDict)[[1]]

##Question 3 : PCA 

###run PCA: input the snippets of text, counting the appropriate features

snippets_pca<-prcomp(snippets_dfm, center=TRUE, scale.=TRUE)

##examine number of components

plot(snippets_pca, type = "l")

##packages for visualization--code taken from http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
#
#library(devtools)
#install_github("ggbiplot", "vqv")
#library(ggbiplot)

g <- ggbiplot(snippets_pca, obs.scale = 1, var.scale = 1, 
              groups = authors)
g<- g + theme(legend.direction = 'horizontal', 
              legend.position = 'top')
g



##Predict : input the dfm (with appropriate features) of the mystery text

predicted<-predict(snippets_pca, newdata=mystery_dfm)

##Fisher's linear discrimination rule: choose the group that has a closer group mean; just 2 dimensions

d <- dickens_snippets@Dim[1]
a <- austen_snippets@Dim[1]

#find the mean of the first two PCs 
austen_pc1_mean<-mean(snippets_pca$x[1:a,1])
austen_pc2_mean<-mean(snippets_pca$x[1:a,2])
austen_mean <- c(austen_pc1_mean, austen_pc2_mean)

dickens_pc1_mean<-mean(snippets_pca$x[(a+1):(a+d),1])
dickens_pc2_mean<-mean(snippets_pca$x[(a+1):(a+d),2])
dickens_mean<-c(dickens_pc1_mean, dickens_pc2_mean)


mystery_pc1_mean<-mean(predicted[,1])
mystery_pc2_mean<-mean(predicted[,2])
mystery_mean<-c(mystery_pc1_mean, mystery_pc2_mean)

## now you need to find which is closer to the mystery mean
mystery_dickens_dist = dist(rbind(mystery_mean, dickens_mean)) # 5.997075
mystery_austen_dist = dist(rbind(mystery_mean, austen_mean)) # 3.251736

# Question 4 Zipf's Law and Heap's Law

# Zipf's Law

mydfm <- dfm(textCorpus)
plot(log10(1:100), log10(topfeatures(mydfm, 100)),
     xlab="log10(rank)", ylab="log10(frequency)", main="Zipf's Law: Top 100 Words")
# regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(mydfm, 100)) ~ log10(1:100))
abline(regression, col="red")
confint(regression)

# Heap's Law

# M = kT^b

# M = numTypes = vocab size
# T = numTokens = number of tokens
# k = 44
# Find b

numTypes = length(features(mydfm)) # 31198
tokens <- tokenize(textCorpus, removePunct=TRUE) 
numTokens <- sum(unlist(lapply(tokens, length))) # 1812844
# solve for b
b = log(numTypes/44)/log(numTokens)
b # 0.4554985
# Check if it's close
estimatedNumTypes = 44*(numTokens^b)
numTypes-estimatedNumTypes

# Question 5
kwic(textCorpus, "class", 6)
