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

mypwd = file.path('data/source')

docs = Corpus(DirSource(mypwd))

# clean the text data by stripping out special characters, putting all words
# in lower case, removing stop words, and stemming data
toSpace = content_transformer(function(yourdata, target) gsub(target, ' ', yourdata))
docs = tm_map(docs, toSpace, "/")
docs = tm_map(docs, toSpace, "@")
docs = tm_map(docs, toSpace, ";")
docs = tm_map(docs, toSpace, "--")
docs = tm_map(docs, toSpace, " +")

docs <- tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, stopwords("english"))
docs = tm_map(docs,stemDocument)
docs = tm_map(docs,stripWhitespace)
docs = tm_map(docs,PlainTextDocument)

# Remove empty documents
for(j in seq(docs))   
{   
  docs[[j]]$content <- docs[[j]]$content[which(docs[[j]]$content != "")]
}   

# Create and save Document Term Matrix, Term Document Matrix, and save off
# documents
dtm = DocumentTermMatrix(docs)
tdm = TermDocumentMatrix(docs)

save(dtm, file="data/dtm.Rda")
save(tdm, file="data/tdm.Rda")
save(docs, file = "data/docs.Rda")

