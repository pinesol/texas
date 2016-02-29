# Question 6: filtering text

# TODO split the text up by sentences (use quanteda's tokenize?)
# TODO Use their code below to filter the text more.
# TODO Add this into a data frame in such a way that the years have a column
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

#TODO don't use this exact line--think about how it's different
man_df<-data.frame(year = as.numeric(files), text = text , stringsAsFactors = FALSE)



##apply to your data frame of the texts of the manifestos 
# TODO I think this can be used to filter out filler

df<-filter(df, grepl("^\\?", df$text)==FALSE &grepl("^\\d", df$text) ==FALSE)
df<-filter(df, ntoken(df$text)>3)




