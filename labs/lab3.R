####Recitation 3

## Some Examples taken wholesale from Ken Benoit's NYU Dept. of Politics short course Fall 2014
## Avaliable on his website: www.kenbenoit.net


library(quanteda)
library(quantedaData)

##load data




##load in data
data("iebudgetsCorpus")


df<-data.frame(iebudgetsCorpus$documents)

## Lexical diversity measures

# TTR 
tokens<-tokenize(iebudgetsCorpus, removePunct=TRUE) 
tokens[1]
tokenz<-lapply(tokens,  length )
tokenz[1]
typez<-lapply(lapply(tokens,  unique ), length)
typez[1]
TTRz<-mapply("/",typez,tokenz,SIMPLIFY = FALSE)
TTRz[1]
df$ttr<-unlist(TTRz)



##basic plot

plot(df$ttr)


## 

df$year<-as.numeric(df$year)

aggregate(df$ttr, by=list(df$year), FUN=mean)

table(df$year)


aggregate(df$ttr, by=list(df$party), FUN=mean)



table(df$party)




##Let's think about if it matters


#readability measure

?readability


##let's look at FRE
df$read_FRE<-readability(df$texts, "Flesch")


aggregate(df$read_FRE, by=list(df$year), FUN=mean)

aggregate(df$read_FRE, by=list(df$party), FUN=mean)

##Dale-Chall measure
df$read_DC<-readability(df$texts, "Dale.Chall")


aggregate(df$read_DC, by=list(df$year), FUN=mean)

aggregate(df$read_DC, by=list(df$party), FUN=mean)

##let's look at all of em

read<-readability(df$texts)
# correlation
cor(read$Flesch, read$Dale.Chall)

cor(read$Flesch, read$SMOG)

cor(read$Coleman.Liau, read$Dale.Chall)










cor(read$Fucks, read$Dale.Chall)


####Bootstrapping!


#Let's consider the different speeches by different parties/years, say we want to get standard errors on Flesch scores
install.packages("dplyr")
library(dplyr)

##initialize data frames
year_FRE<-data.frame(matrix(ncol = 5, nrow = 100))


#Let's filter out the parties with only one speech

df<-filter(df, party != "WUAG" & party != "SOC"  & party != "PBPA" )

party_FRE<-data.frame(matrix(ncol = 6, nrow = 100))


# NOTE: not doing sentence level bootstrapping b.c. that's what we do in our homework
# only doing bootstrapping on the speech level here.

#run the bootstraps

for(i in 1:100){
  
  # new df: sample 200 with replacement
  bootstrapped<-sample_n(df, 200, replace=TRUE)
  
  bootstrapped$read_FRE<-readability(bootstrapped$texts, "Flesch")
  
  # store results in a row of each of the result data frames
  
  year_FRE[i,]<-aggregate(bootstrapped$read_FRE, by=list(bootstrapped$year), FUN=mean)[,2]
  
  party_FRE[i,]<-aggregate(bootstrapped$read_FRE, by=list(bootstrapped$party), FUN=mean)[,2]
  
}

#name the data frames
# table() aggregates the list into a histogram
colnames(year_FRE)<-names(table(df$year))
colnames(party_FRE)<-names(table(df$party))

#define the standard error function
std <- function(x) sd(x)/sqrt(length(x))

##calculate standard errors and point estimates

year_ses<-apply(year_FRE, 2, std)

year_means<-apply(year_FRE, 2, mean)


party_ses<-apply(party_FRE, 2, std)

party_means<-apply(party_FRE, 2, mean)


###Plot results--year
# NOTE: install plotting package
install.packages("ggplot2") # Actually he doesn't use this plotting library here.

coefs<-year_means
ses<-year_ses

y.axis <- c(1:5)
min <- min(coefs - 2*ses)
max <- max(coefs + 2*ses)
var.names <- colnames(year_FRE)
adjust <- 0
par(mar=c(2,8,2,2))

plot(coefs, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .8, 
     xlim=c(min,max),ylim = c(.5,6.5), main = "")
rect(min,.5,max,1.5, col = c("grey97"), border="grey90", lty = 2)
rect(min,1.5,max,2.5, col = c("grey95"), border="grey90", lty = 2)
rect(min,2.5,max,3.5, col = c("grey97"), border="grey90", lty = 2)
rect(min,3.5,max,4.5, col = c("grey95"), border="grey90", lty = 2)
rect(min,4.5,max,5.5, col = c("grey97"), border="grey90", lty = 2)
#rect(min,5.5,max,6.5, col = c("grey97"), border="grey90", lty = 2)

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

##real world data
table(df$year)

aggregate(df$read_FRE, by=list(df$year), FUN=mean)


###Plot results--party

coefs<-party_means
ses<-party_ses

y.axis <- c(1:6)
min <- min(coefs - 2*ses)
max <- max(coefs + 2*ses)
var.names <- colnames(party_FRE)
adjust <- 0
par(mar=c(2,8,2,2))

plot(coefs, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .8, 
     xlim=c(min,max),ylim = c(.5,6.5), main = "")
rect(min,.5,max,1.5, col = c("grey97"), border="grey90", lty = 2)
rect(min,1.5,max,2.5, col = c("grey95"), border="grey90", lty = 2)
rect(min,2.5,max,3.5, col = c("grey97"), border="grey90", lty = 2)
rect(min,3.5,max,4.5, col = c("grey95"), border="grey90", lty = 2)
rect(min,4.5,max,5.5, col = c("grey97"), border="grey90", lty = 2)
rect(min,5.5,max,6.5, col = c("grey97"), border="grey90", lty = 2)

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


##real world data
table(df$party)
aggregate(df$read_FRE, by=list(df$party), FUN=mean)


df$browseVignettes(package = "dplyr")