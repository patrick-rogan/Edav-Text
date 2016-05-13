require('wordcloud')
require('biclust')
require('cluster')
require('igraph') 
require('dplyr')
require('scales')
require('SnowballC')
require('RColorBrewer')
require('ggplot2')
require('tm')
require('Rgraphviz')
require('fpc')
require('topicmodels')

### directory 
setwd("data/")


### load data
load('dtm.Rda')
presidents = read.csv('President_info3.csv')

### text_president dictionary stores word count information of each president over years
president_names <- as.character(unique(presidents$name))
text_president <- vector(mode="list", length=length(president_names))
for (i in 1:length(president_names)){
  index = presidents[presidents$name==president_names[i],]$index
  text_president[[i]] = dtm[index,]
}
names(text_president) <- president_names

### freq_president calculates and stores the word count of each president's speeches
freq_president <- vector(mode="list", length=length(president_names))
for (i in 1:length(president_names)){
  freq_president[[i]] = colSums(as.matrix(text_president[[i]]))
}  
names(freq_president) <- president_names


### top10word_president extracts the top 10 words and the relevant count under each president
top10word_president <- vector(mode="list", length=length(president_names))
word_union <- c()
for (i in 1:length(president_names)){
  top10word_president[[i]] = sort(freq_president[[i]][freq_president[[i]]>0], decreasing = T)[0:10]
  word_union = union(word_union,names(top10word_president[[i]]))
}  
names(top10word_president) <- president_names

### top10word_matrix is a 41 by 60 dimension matrix where 41 stands as numbers of preseidents
### and 60 represents the union of the frequently using words on their speech. 
top10word_matrix <- matrix(0, nrow = length(president_names), ncol = length(word_union))
rownames(top10word_matrix) <- president_names
colnames(top10word_matrix) <- word_union
for (i in 1:length(president_names)){
  words_tmp = names(top10word_president[[i]])
  for (j in 1:10){
     word_tmp = words_tmp[j]
     word_idx = which(word_union==word_tmp)
     top10word_matrix[i,word_idx] = top10word_president[[i]][[j]]
  }
}

## save the matrix for later visualization 
save(top10word_matrix,file="data/top10word_matrix.Rda")


