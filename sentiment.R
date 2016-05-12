require('dplyr')
require('SnowballC')
require('tm')
require('xlsx')
require('zoo')
require('plyr')
require('plotly')

# Read in data, the corpus of speeches and presidental information 
load("dtm.Rda")
load("tdm.Rda")
load("docs.Rda")
information = read.csv('President_Info3.csv', header=T)

# Read in recession data and then strip out irrelevant lines
recessions = read.xlsx('data/NBER_chronology.xlsx',sheetIndex =1, header=T)
recessions = recessions[2:34,] 

# Convert factors to yearmon format then assume the recession started and ended
# on the 15th day of the month. We justify this as the idea of a recession 
# start/stop date is not exact and we do not anticipate this having a meaningful
# impact on the results. 
recessions$Peak.month = as.Date(as.yearmon(recessions$Peak.month)) + 14
recessions$Trough.month = as.Date(as.yearmon(recessions$Trough.month)) + 14
recessions = recessions[,1:2]

# Get speech dates, strip out irrelevant characters, convert to date,
# then add a binary variable that will represent whether the nation
# was in a recession at the time.
speechDates = list.files('source/')
speechDates = sub('.txt','',speechDates)
speechDates = gsub('_',' ',speechDates)
speechDates = as.data.frame(as.Date(speechDates, format = '%Y %b %d'))
speechDates[,2] = 0
colnames(speechDates) = c("Speech_Date", "In_Recession")

# Simple function that checks if the speech date is within the start/stop
# dates of every recession. Return true on the first instance of the speech
# being in a period of recession, else return false.
inRecession <- function(date,intervals){
  for (i in 1:dim(intervals)[1]){
    if (date >= intervals[i,1] && date <= intervals[i,2]){
      return(1)
    }
  }
  return(0)
}

# For each speech date, label the speech 1 if it occurred during a recession, 0 otherwise
for (i in 1:length(speechDates$Speech_Date)){
  speechDates$In_Recession[i] = inRecession(speechDates$Speech_Date[i], recessions)
}

# Get the indices corresponding to whether or not the speech was delivered during a recession
# note that we only consider dates on of afer 1857-06-15, the time period for which we have data.
recessionIndices = which(speechDates$Speech_Date >= '1857-06-15' & speechDates$In_Recession == 1)
normalIndices = which(speechDates$Speech_Date >= '1857-06-15' & speechDates$In_Recession == 0)

# Sentiment analysis based on: 
# 
# http://www.r-bloggers.com/sentiment-analysis-on-donald-trump-using-r-and-tableau/
#   With positive/negative words from:
#   
#   Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews." 
#       Proceedings of the ACM SIGKDD International Conference on Knowledge 
#       Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, 
#       Washington, USA

positives= readLines("data/positive-words.txt")
negatives = readLines("data/negative-words.txt")

# Merge strings of speech content to one string containing all the words in the speech
fullSpeeches = list()
for (i in 1:length(docs)){
  fullSpeeches[i] = paste(docs[[i]]$content, sep="", collapse=" ")
}

# Calculate simple sentiment scores. This function compares the words 
# in a speech against two corpora of positive and negative words and then
# takes the difference between positive and negative words
sentiment_scores = function(speech, positive_words, negative_words){
  scores = c()
  for (i in 1:length(speech)){
    word_list = strsplit(as.character(speech[i]), " +")
    words = unlist(word_list)
    # compare words to the dictionaries of positive & negative terms
    positive.matches = match(words, positive_words)
    negative.matches = match(words, negative_words)
    # get the position of the matched term or NA
    # we just want a TRUE/FALSE
    positive_matches = !is.na(positive.matches)
    negative_matches = !is.na(negative.matches)
    # final score
    scores[i] = sum(positive_matches) - sum(negative_matches)
  }
  return(scores)
}

# Calculate sentiment scores for each speech according to the economic conditions 
# when it was delivered
scoreRecession = sentiment_scores(fullSpeeches[recessionIndices], positives, negatives)
scoreNormal = sentiment_scores(fullSpeeches[normalIndices], positives, negatives)

png("figs/S_Econ.png",width=800,height=500, unit= 'px')
p1 <- hist(scoreRecession,xlab="Sentiment Score",main="State of the Union Sentiment by Economic Condition",
     border="black",col=rgb(1,0,0,0.5), xlim=c(-50,400), ylim = c(0,42))
hist(scoreNormal, col=rgb(0,0,1,0.5), add=T)
legend("topright", c("Normal Conditions","Recession"), fill=c("blue", "red"))
box()
dev.off()

# Now do a similar analysis for the democratic and republican parties. Note that these
# two parties have only existed in their current incarnation since 1852.
index_republican = information$index[which(information$Political.Party == "Republican" & information$year_x >= 1852)]
index_democrat = information$index[which(information$Political.Party == "Democrat" & information$year_x >= 1852)]

scoreRepublican = sentiment_scores(fullSpeeches[index_republican], positives, negatives)
scoreDemocrat = sentiment_scores(fullSpeeches[index_democrat], positives, negatives)

png("figs/S_Party.png",width=800,height=500, unit= 'px')
hist(scoreRepublican,xlab="Sentiment Score",main="State of the Union Sentiment by Presidential Party",
     border="black",col=rgb(1,0,0,0.5), xlim=c(-50,400))
hist(scoreDemocrat, col=rgb(0,0,1,0.5),add=T)
legend("topright", c("Democrat", "Republican"), fill=c("blue", "red"))
box()
dev.off()
