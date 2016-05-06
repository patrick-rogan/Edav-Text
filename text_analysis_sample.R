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

setwd("C:\\Users\\HS\\Desktop\\school\\classes\\VIS\\HW_TEXT")

load("dtm.Rda")
load("tdm.Rda")

## loading correspondence
information = read.csv('name_year_month.csv', header=T)

## getting indexes of barac obama
# which(information$Name == "Barack Obama")
# which((information$Year >= 2009) & (information$Year <= 2016))
index = which((information$Year >= 2009) & (information$Year <= 2010))
dtm = dtm[index, ]
tdm = tdm[, index]

## Frequency
num_freq = 200
mfreq = colSums(as.matrix(dtm))
p1 = ggplot(subset(data.frame(word=names(mfreq),freq=mfreq),freq> num_freq),aes(word,freq))+ geom_bar(stat='identity') + theme(axis.text.x=element_text(size=12,color='red',fac='bold.italic',angle=45,hjust=0.5))
plot(p1)

## Wordcount
set.seed(111)
min_freq = 10
wordcloud(names(mfreq),mfreq,
          min.freq=min_freq,
          scale=c(4,0.5),
          colors=brewer.pal(8, "Dark2"),
          random.color=FALSE, 
          random.order=FALSE)

## Frequency relationship
low_freq = 150
cor_Threshold = 0.6
plot(dtm,terms=findFreqTerms(dtm,lowfreq = low_freq),corThreshold = cor_Threshold)


#### Reducing dimension
dtmc = removeSparseTerms(dtm, sparse=0.2) 
tdmc = removeSparseTerms(tdm, sparse=0.2)

## clustering
mdist = dist(tdmc,method='euclidian')
mfit = hclust(d=mdist,method='ward.D2')
plot(mfit,hang=-1)
rect.hclust(mfit,k=5,border='red')


## clustering
mkm = kmeans(mdist,3)
clusplot(as.matrix(mdist),mkm$cluster,color=T,shade=T,labels=2,lines=0)


## First topic identified for every document
dtm2 = as.DocumentTermMatrix(tdmc)
mlda = LDA(dtm2,k=5)
mterms = terms(mlda,4)

mterms = apply(mterms,MARGIN=2,paste,collapse=', ')

mtopic = topics(mlda,1)
mtopics = data.frame(doc=1:dim(dtm)[1],topic1=mtopic)
qplot(doc,..count..,data=mtopics,geom='density',
      fill=mterms[mtopic],position='stack')

