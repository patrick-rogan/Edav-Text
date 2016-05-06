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

#setwd("C:\\Users\\HS\\Desktop\\school\\classes\\VIS\\HW_TEXT")

mypwd = file.path('source')
mypwd

dir(mypwd)
docs = Corpus(DirSource(mypwd))

#inspect(docs)

toSpace = content_transformer(function(yourdata, target) gsub(target, ' ', yourdata))
docs = tm_map(docs, toSpace, "/")
docs = tm_map(docs, toSpace, "@")
#docs = tm_map(docs, toSpace, "(")
#docs = tm_map(docs, toSpace, ")")
docs = tm_map(docs, toSpace, ";")
docs = tm_map(docs, toSpace, "--")

docs <- tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, stopwords("english"))
# docs = tm_map(docs, removeWords,c('school','department'))
docs = tm_map(docs,stemDocument)
docs = tm_map(docs,stripWhitespace)
docs = tm_map(docs,PlainTextDocument)

dtm = DocumentTermMatrix(docs)
tdm = TermDocumentMatrix(docs)


# mfreq = colSums(as.matrix(dtm[1:3, 1:dim(dtm)[2]]))
mfreq = colSums(as.matrix(dtm))
p1 = ggplot(subset(data.frame(word=names(mfreq),freq=mfreq),freq>40),aes(word,freq))+ geom_bar(stat='identity') + theme(axis.text.x=element_text(size=12,color='red',fac='bold.italic',angle=45,hjust=0.5))
plot(p1)

set.seed(111) # if you'd like to make the configuration of the layout consistent each time
wordcloud(names(mfreq),mfreq,
          min.freq=5, # plot words apprear 10+ times
          scale=c(4,0.5), # make it bigger with argument "scale"
          colors=brewer.pal(8, "Dark2"), # use color palettes
          random.color=FALSE, 
          random.order=FALSE)

plot(dtm,terms=findFreqTerms(dtm,lowfreq = 40),corThreshold = 0.5)


dtmc = removeSparseTerms(dtm,sparse=0.01) 
tdmc = removeSparseTerms(tdm,sparse=0.01)

mdist = dist(tdmc,method='euclidian')
mfit = hclust(d=mdist,method='ward.D2')
plot(mfit,hang=-1)
rect.hclust(mfit,k=5,border='red')

mkm = kmeans(mdist,3)
clusplot(as.matrix(mdist),mkm$cluster,color=T,shade=T,labels=2,lines=0)


dtm2 = as.DocumentTermMatrix(tdmc)
mlda = LDA(dtm2,k=5) #find k topics
mterms = terms(mlda,4) # find the first 4 terms of each topic

mterms = apply(mterms,MARGIN=2,paste,collapse=', ')
# First topic identified for every document
mtopic = topics(mlda,1)
mtopics = data.frame(doc=1:8,topic1=mtopic)
qplot(doc,..count..,data=mtopics,geom='density',
      fill=mterms[mtopic],position='stack')

