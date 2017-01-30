rm(list = ls())
setwd("/Users/mgbrooks208/Dropbox/NHS_gorgas")
library(XML)
library('tidyr')
library('dplyr')
library(data.table)
#Cleaning the MUJER data set
# The steps I take below are very specific to this particular file but can be helpful for when you
# ecnounter problems loading data in the future.
# When I first loaded MUJER the columns had the wrong data corresponding to the varible names.
# So I looked through the file as text and there were ***'s.
# I used regular expressions to remove them. Then take lines the lines of text and out them
# in a dataframe. I use lapply to go through each line a text and split on the commas 
# This gives us a list vectors of strings. The list is of length 5619 because that is the number
# of observations. The lenght of the elements of the list dat 
# (which can be observed by legnth(dat[[i]])) should be the number of variables. But as we can see
# there are a few with different lengths. Most have length 2772. Some have 2773,2774,2775

# Here we load the data in as raw text. Then I use regular expressions to remove any case of a "*".
# This is know as a "meta character", there a few others you should look up for future cases. I use
# regular expressions to remove the meta character. The expression "[*+]" identifies any case of 1 or 
# more *, which I remove by substituting with nothing ('').
w_dat <- readLines(textConnection('MUJER.csv'))
w_dat <- sapply(w_dat,function(x){gsub('[*+]','',x)})
dat <- lapply(1:length(w_dat),function(x){strsplit(w_dat[x],split=',')[[1]]})
# to get length of each observation
lengths = sapply(dat,length)
# To see which ones have different length
which(!(lengths == 2772))
# We remove the ends of these because we dont need them and it is probably an error.
# Since we only need the questions that came way earlier in the questionaire I remove the tail variables
# (numbers 2773,2774,2775). Note, only do this if you are sure you dont need that information.
for (i in which(!lengths == 2772)){
  if (length(dat[[i]]) == 2775){
    dat[[i]] <- dat[[i]][-1*c(2773,2774,2775)]
  }
  else if(length(dat[[i]]) == 2774){
    dat[[i]] <- dat[[i]][-1*c(2773,2774)]}
  else if (length(dat[[i]]) == 2773){
    dat[[i]] <- dat[[i]][-1*2773]}
}
# Right now our data is all in text form and in a list. Each element of the list is an
# row of the data (an observation from the questionaire). do.call is an efficient way
# to combine each element of the list into one data structure. Then we use the data.frame
# function to turn it into a dataframe.
m <- do.call("rbind",dat)
df <- as.data.frame(m,stringsAsFactors = FALSE)
# We remove the first row because these are the variable names and shouldnt be an element of the dataframe.
df <- df[-1*1,]
# The column names are the variable names so we set the column names equal to what as the first row
# of the data set.
colnames(df) <- m[1,]
row.names(df) <- NULL
#Now our data is in the right format and we can write it as a csv file so we can perform the proper 
# analysis.
write.csv(df, file = "N_MUJER.csv",row.names = FALSE)