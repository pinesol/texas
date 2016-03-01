# Question 6: filtering text

# DONE split the text up by sentences (use quanteda's tokenize?)
# DONE Use their code below to filter the text more.
# DONE Add this into a data frame in such a way that the years have a column
# TODO bootstrap using code from lab2.
# TODO make a graph
# TODO print means of bootstrapped FRE scores
# TODO print means of observed in the data (?)
# TODO calculate more FRE and DC scores and report their correlation?

library(dplyr)

library(quanteda)
library(quantedaData)
##read in conservative manifestos

setwd("~/texas/hw1/cons")

##read in the files
files <- list.files(full.names=TRUE)
text <- lapply(files, readLines)
text <- unlist(lapply(text, function(x) paste(x, collapse = " ")))

#name data
files<-unlist(files)
files<-gsub("./Con", "", files )
files<-gsub(".txt", "", files )

# Split text and years into sentences
sentence_text = character()
sentence_files = numeric()
for (i in 1:length(text)) {
  sentences = tokenize(text[i], simplify = TRUE, what = 'sentence')
  sentence_text = c(sentence_text, sentences)
  for (j in 1:length(sentences)) {
    sentence_files = c(sentence_files, files[i])
  }
}

df<-data.frame(year = sentence_files, text = sentence_text , stringsAsFactors = FALSE)

##apply to your data frame of the texts of the manifestos 

df<-filter(df, grepl("^\\?", df$text)==FALSE &grepl("^\\d", df$text) ==FALSE)
df<-filter(df, ntoken(df$text)>3)

kNumBootstraps = 100
kSampleSize = 5000

year_FRE<-data.frame(matrix(ncol = length(files), nrow = kNumBootstraps))

# Run Bootstrap
for (i in 1:kNumBootstraps) {
  print(i)
  bootstrapped<-sample_n(df, kSampleSize, replace=TRUE)
  bootstrapped$read_FRE<-readability(bootstrapped$text, "Flesch")
  # store results in a row of each of the result data frames
  year_FRE[i,]<-aggregate(bootstrapped$read_FRE, by=list(bootstrapped$year), FUN=mean)[,2]
}

#name the data frames
# table() aggregates the list into a histogram
colnames(year_FRE)<-names(table(df$year))

#define the standard error function
std <- function(x) sd(x)/sqrt(length(x))

##calculate standard errors and point estimates

year_ses<-apply(year_FRE, 2, std)
year_means<-apply(year_FRE, 2, mean)

###Plot results--year
# NOTE: install plotting package
install.packages("ggplot2") # Actually he doesn't use this plotting library here.

coefs<-year_means
ses<-year_ses

y.axis <- c(1:length(files))
min <- min(coefs - 2*ses)
max <- max(coefs + 2*ses)
var.names <- colnames(year_FRE)
adjust <- 0
par(mar=c(2,8,2,2))

plot(coefs, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .8, 
     xlim=c(min,max),ylim = c(.5,0.5+length(files)), main = "")
for (i in 1:length(files)) {
  rect(min,(i-1)+.5,max,i+.5, col = c("grey97"), border="grey90", lty = 2)  
}

axis(1, at = seq(min,max,(max-min)/10), 
     labels = c(round(min+0*((max-min)/10),3),
                round(min+1*((max-min)/10),3),
                round(min+2*((max-min)/10),3),
                round(min+3*((max-min)/10),3),
                round(min+4*((max-min)/10),3),
                round(min+5*((max-min)/10),3),
                round(min+6*((max-min)/10),3),
                round(min+7*((max-min)/10),3),
                round(min+8*((max-min)/10),3),
                round(min+9*((max-min)/10),3),
                round(max,3)),tick = T,cex.axis = .75, mgp = c(2,.7,0))
axis(2, at = y.axis, label = var.names, las = 1, tick = FALSE, cex.axis =.8)
abline(h = y.axis, lty = 2, lwd = .5, col = "white")
segments(coefs-qnorm(.975)*ses, y.axis+2*adjust, coefs+qnorm(.975)*ses, y.axis+2*adjust, lwd =  1)

segments(coefs-qnorm(.95)*ses, y.axis+2*adjust-.035, coefs-qnorm(.95)*ses, y.axis+2*adjust+.035, lwd = .9)
segments(coefs+qnorm(.95)*ses, y.axis+2*adjust-.035, coefs+qnorm(.95)*ses, y.axis+2*adjust+.035, lwd = .9)
points(coefs, y.axis+2*adjust,pch=21,cex=.8, bg="white")

# Get the means observed in data, and plot those too

allYears_FRE<-readability(df$text, "Flesch")
# store results in a row of each of the result data frames
allYears_FRE_agg <- aggregate(allYears_FRE, by=list(df$year), FUN=mean)[,2]

points(allYears_FRE_agg, y.axis+2*adjust,pch=21,cex=.8, bg="gray")

# Get FRE and DC score for each year
unsplit_df <- data.frame(year = files, text = text , stringsAsFactors = FALSE)
unsplit_FRE <- readability(unsplit_df$text, "Flesch")


