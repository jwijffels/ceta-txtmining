load("ceta.RData")
#ceta$ceta_nouns <- subset(ceta$ceta_nouns, !word.lemma %in% c("agreement", "paragraph", "chapter", "note", "law", "article"))

######################################################################################
## Look at POS tags
##
######################################################################################
library(lattice)
barchart(sort(table(ceta$ceta_tagged$word.type)), col = "lightblue", xlab = "Term frequency", 
         main = "Parts of Speech Tag term frequency\n in CETA treaty")

######################################################################################
## Build a topic model
##
######################################################################################
library(topicmodels)
library(topicmodels.utils)
library(slam)
x <- ceta$ceta_nouns[, list(freq = .N), by = list(document = article.id, term = word.lemma)]
dtm <- document_frequency_matrix(x)
topterms <- col_sums(dtm)
topterms <- topterms[topterms > 30]
topterms <- head(topterms, 250)
topterms <- names(topterms)
dtm <- dtm[, topterms]
dtm <- dtm[row_sums(dtm) > 0, ]
set.seed(123456789)
ceta_topics <- LDA(x = dtm[, topterms], k = 5, method = "VEM", 
                   control = list(alpha = 0.1, estimate.alpha = TRUE, seed = as.integer(10:1), 
                                  verbose = FALSE, nstart = 10, save = 0, best = TRUE))
ceta_topic_terms <- predict(ceta_topics, type = "terms", min_posterior = 0.01)

######################################################################################
## Visualise
##
######################################################################################

##
## Frequency statistics
##
x <- ceta$ceta_nouns[, list(n = .N), by = list(word.lemma)]
x <- x[order(x$n, decreasing = TRUE), ]
x <- as.data.frame(x)



library(wordcloud)
wordcloud(words = x$word.lemma, freq = x$n, max.words = 150, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

library(wordcloud2)
wordcloud2(data = head(x, 700), figPath = "maple_europa_black.png")

library(topicmodels.utils)
x$word.lemma <- factor(x$word.lemma, 
                       levels = rev(x$word.lemma))
bareffects(head(x, 40), panel = "CETA word usage", cextext = 0.8, addpct = FALSE, 
           xlab = "Word frequency in CETA treaty")

##
## Co-occurrence statistics (how many times are 2 nouns occurring across the article treaties)
##
library(ggraph)
library(ggforce)
library(igraph)
library(tidytext)
word_cooccurences <- pair_count(data=ceta$ceta_nouns, group="article.id", value="word.lemma", sort = TRUE)
set.seed(123456789)
head(word_cooccurences, 70) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8, col = "darkgreen") +
  ggtitle(sprintf("\n%s", "CETA treaty\nCo-occurrence of nouns")) +
  theme_void()

##
## Word correlations (correlations of word usage between top 100 nouns)
##
library(topicmodels.utils)
idx <- predict(ceta_topics, dtm, type = "topics")
idx <- idx$topic == 3
termcor <- termcorrplot(dtm[idx, ], 
                        words = names(ceta_topic_terms$topic3), highlight = head(names(ceta_topic_terms$topic3), 3),
                        autocorMax = 25, lwdmultiplier = 3, drawlabel = TRUE, cex.label = 0.8)

library(Matrix)
library(qgraph)
terms <- predict(ceta_topics, type = "terms", min_posterior = 0.025)
terms <- unique(unlist(sapply(terms, names)))
out <- dtm[, terms]
out <- cor(as.matrix(out))
out <- nearPD(x=out, corr = TRUE)$mat
out <- as.matrix(out)

m <- EBICglasso(out, n = 1000)
qgraph(m, layout="spring", labels = colnames(out), label.scale=FALSE,
       label.cex=1, node.width=.5)

##
## Word networks
##
library(semnet)
terms <- unique(unlist(sapply(ceta_topic_terms, names)))
cooc <- coOccurenceNetwork(dtm[, terms])
cooc <- simplify(cooc)
plot(cooc, vertex.size=V(cooc)$freq / 20, edge.arrow.size=0.5)

##
## Interactive visualisations - word network
##
library(tidytext)
library(visNetwork)
terms <- lapply(ceta_topic_terms, FUN=function(x) head(x, 7))
terms <- names(terms$topic5)

nodes <- ceta$ceta_nouns[word.lemma %in% terms, list(value = .N), by = list(id = word.lemma, label = word.lemma)]
nodes$id <- 1:nrow(nodes)
edges <- pair_count(data=subset(ceta$ceta_nouns, word.lemma %in% terms), 
                    group="article.id", value="word.lemma", sort = TRUE)
colnames(edges) <- c("from", "to", "value")
edges$from <- mapvalues(edges$from, from = nodes$label, to = nodes$id)
edges$to <- mapvalues(edges$to, from = nodes$label, to = nodes$id)
visNetwork(nodes, edges, main = "Topic 5, word coocurrence network") %>% 
  visOptions(highlightNearest = TRUE)  %>% 
  visLegend() %>%
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)



##
## Topic plots
##
library(LDAvis)
json <- createJSON(phi = posterior(ceta_topics)$terms,
                   theta = posterior(ceta_topics)$topics,
                   doc.length = row_sums(dtm),
                   vocab = colnames(dtm),
                   term.frequency = col_sums(dtm))
serVis(json)



