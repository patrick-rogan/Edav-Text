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

docs = Corpus(DirSource(mypwd))

#inspect(docs)

toSpace = content_transformer(function(yourdata, target) gsub(target, ' ', yourdata))
docs = tm_map(docs, toSpace, "/")
docs = tm_map(docs, toSpace, "@")
#docs = tm_map(docs, toSpace, "(")
#docs = tm_map(docs, toSpace, ")")
docs = tm_map(docs, toSpace, ";")
docs = tm_map(docs, toSpace, "--")
docs = tm_map(docs, toSpace, " +")

docs <- tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, stopwords("english"))
# docs = tm_map(docs, removeWords,c('school','department'))
docs = tm_map(docs,stemDocument)
docs = tm_map(docs,stripWhitespace)
docs = tm_map(docs,PlainTextDocument)

for(j in seq(docs))   
{   
  docs[[j]]$content <- docs[[j]]$content[which(docs[[j]]$content != "")]
}   

dtm = DocumentTermMatrix(docs)
tdm = TermDocumentMatrix(docs)

save(dtm, file="dtm.Rda")
save(tdm, file="tdm.Rda")
save(docs, file = "docs.Rda")

#load("dtm.Rda")
#load("tdm.Rda")

