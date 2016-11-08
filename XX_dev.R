##########################################################################################
## UNDER CONSTRUCTION
##
##########################################################################################
library(Rgraphviz)
AgNodes(termcor)
as_data_frame(edge(), what = "edges")
termcor %>% AgEdge %>%  qgraph
buildEdgeList(edge(AgEdge(termcor)))

x <- ceta$ceta_nouns[, list(freq = .N), by = list(document = article.id, term = word.lemma)]
scores <- predict(ceta_topics, 
                  newdata = document_frequency_matrix(x, vocabulary = ceta_topics@terms), 
                  type = "topics")
articles_onlytopic_2_and_3 <- scores$document[scores$topic %in% c(2, 3)]
word_cooccurences <- pair_count(data=subset(ceta$ceta_nouns, article.id %in% articles_onlytopic_2_and_3), 
                                group="article.id", value="word.lemma", sort = TRUE)


library(igraph)
terms <- names(ceta_topic_terms$topic3[ceta_topic_terms$topic3 > 0.025])
out <- dtm[, terms]
out <- cor(as.matrix(out))
out <- nearPD(x=out, corr = TRUE)$mat
out <- as.matrix(out)
m <- EBICglasso(out, n = 1000)

ig <- graph.adjacency(m, mode="undirected", weighted=TRUE)
plot(ig, edge.label=round(E(ig)$weight, 3))