### packages 
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
# source("https://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
require('Rgraphviz')
require('fpc')
require('topicmodels')

### directory 
setwd("/Users/yaoruyi/Documents/Columbia/Spring 2016/STAT W4701 EDAV/Project4/Edav-Text")


### load data
load('dtm.Rda')
presidents = read.csv('President_info3.csv')


### build some dictionaries 
president_names <- levels(presidents$name)

text_president <- vector(mode="list", length=length(president_names))
for (i in 1:length(president_names)){
  index = presidents[presidents$name==president_names[i],]$index
  text_president[[i]] = dtm[index,]
}
names(text_president) <- president_names

freq_president <- vector(mode="list", length=length(president_names))
for (i in 1:length(president_names)){
  freq_president[[i]] = colSums(as.matrix(text_president[[i]]))
}  
names(freq_president) <- president_names

### word cloud 
#### this part will save plots on your current directory 
set.seed(111) 
for (i in 1:length(president_names)){
  freq_tmp = freq_president[[i]]
  name_tmp = president_names[i]
  year_tmp = range(presidents[presidents$name==president_names[i],]$year_x)
  
  png(paste(name_tmp,".png"), width=12, height=8, units="in", res=300)
  
  layout(matrix(c(1, 2),nrow=2), heights=c(1, 11))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5,y=0.5,cex=2,paste(name_tmp,"from",year_tmp[1],"to",year_tmp[2]))
  wordcloud(names(freq_tmp),freq_tmp,
            min.freq=quantile(freq_tmp[freq_tmp>0])[[4]], # 75% quantile of the frequency statistics 
            scale=c(4,0.5), 
            colors=brewer.pal(8, "Dark2"), 
            random.color=FALSE, 
            random.order=FALSE,main="Title")  
  dev.off()
}