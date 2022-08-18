library(tidytext)
library(tidyr)
library(jsonlite)
library(tibble)
library(dplyr)
library(tm)
library(topicmodels)
library(ggplot2)

####Getting edge list from retweets----
#twitter edge list function from molly asher + viktoria

library(magrittr)
library(igraph)

# function to find rts
# Contains an option ##commented out to write to file
finding.rts <- function (input.file, debate.name) { 
  
  # Read in file
  tweets <-  read.csv(input.file)
  # Remove space between RT and first @ so as to allow processing
  tweets$text <- gsub ("RT @", "RT@", tweets$text)
  
  library(qdapRegex)
  # Extract the names of the users being retweeted in a retweet
  # NOTE pattern ": @." is just a retweet of a tweet with the pattern ".@" 
  # which is used to make a reply, denoted by @, visibile to everyone
  retweets.of.retweets <- rm_between(tweets$text, ": @", "\\s", extract = T)
  retweets.of.retweets <- sapply( retweets.of.retweets, paste0, collapse="")
  
  # Extract the straight forward retweets
  retweets <- rm_between(tweets$text, "RT@", "\\s", extract = T)
  retweets <- sapply( retweets, paste0, collapse="")
  
  # Create a dataframe containing both retweet and retweet of retweet info
  df <- data.frame (user = tweets$user_username,
                    text = tweets$text,
                    retweets.of.retweets = retweets.of.retweets,
                    retweets = retweets)
  
  # Remove rows with NA values
  df <- df[df$retweets!="NA",] 
  # Extract the retweet rows
  retweets <- df [c(1,4)]
  # Extract the retweet of retweet rows
  retweets.of.retweets <- df [c(1,3)]
  colnames(retweets.of.retweets)[2] <- 'retweets'
  retweets.of.retweets <- retweets.of.retweets[retweets.of.retweets$retweets!="NA",] 
  
  # bind the two files together
  retweets <- rbind (retweets, retweets.of.retweets)
  
  # Remove the colons at the end
  retweets$retweets <- gsub(":\\b", "", retweets$retweets)
  # Remove the commas at the end
  retweets$retweets <- gsub(",\\b", "", retweets$retweets)
  # Remove the fullstops at the end
  retweets$retweets <- gsub("[.]+$", "", retweets$retweets)
  
  rts <- retweets
  
  # Find the number of occurrences of each pair
  retweets2 <- dplyr::count(retweets, user, retweets, sort = T)
  
  # Format for Gephi (Source = person who retweeted, target = person being retweeted)
  colnames(retweets2)[c(1,2,3)] <- c("Source", "Target", "Weight")
  retweets2$Type <- 'Directed'
  # retweets2$Source <- as.character(retweets$Source)  
  #  retweets2$Target <- as.character(retweets$Target)  

  #bad.vs<-V(g)[degree(g) < 2] 
  
  return(list(retweets2, retweets))
  # Write to files
   write.csv(retweets2,file = (debate.name), row.names=F)
}

path = "./tweets/COP/"

cop20.rtwts=finding.rts(paste0(path, "tweetsCOP20.csv"), "COP20retweets.csv")[[1]]
write.csv(cop20.rtwts, file=paste0("./SNA/", "COP20retweets.csv"), row.names = FALSE)

cop22.rtwts=finding.rts(paste0(path, "tweetsCOP22.csv"), "COP22retweets.csv")[[1]]
write.csv(cop22.rtwts, file=paste0("./SNA/", "COP22retweets.csv"), row.names = FALSE)

cop23.rtwts=finding.rts(paste0(path, "tweetsCOP23.csv"), "COP23retweets.csv")[[1]]
write.csv(cop23.rtwts, file=paste0("./SNA/", "COP23retweets.csv"), row.names = FALSE)


finding.rtsPL <- function (path = "./tweets/COP/", pattern, debate.name) { 
  
  # Read in file
  files = list.files(path, pattern, full.names = TRUE)
  tweets = do.call(rbind, lapply(files, function(x) read.csv(x)))
  # Remove space between RT and first @ so as to allow processing
  tweets$text <- gsub ("RT @", "RT@", tweets$text)
  
  library(qdapRegex)
  # Extract the names of the users being retweeted in a retweet
  # NOTE pattern ": @." is just a retweet of a tweet with the pattern ".@" 
  # which is used to make a reply, denoted by @, visibile to everyone
  retweets.of.retweets <- rm_between(tweets$text, ": @", "\\s", extract = T)
  retweets.of.retweets <- sapply( retweets.of.retweets, paste0, collapse="")
  
  # Extract the straight forward retweets
  retweets <- rm_between(tweets$text, "RT@", "\\s", extract = T)
  retweets <- sapply( retweets, paste0, collapse="")
  
  # Create a dataframe containing both retweet and retweet of retweet info
  df <- data.frame (user = tweets$user_username,
                    text = tweets$text,
                    retweets.of.retweets = retweets.of.retweets,
                    retweets = retweets)
  
  # Remove rows with NA values
  df <- df[df$retweets!="NA",] 
  # Extract the retweet rows
  retweets <- df [c(1,4)]
  # Extract the retweet of retweet rows
  retweets.of.retweets <- df [c(1,3)]
  colnames(retweets.of.retweets)[2] <- 'retweets'
  retweets.of.retweets <- retweets.of.retweets[retweets.of.retweets$retweets!="NA",] 
  
  # bind the two files together
  retweets <- rbind (retweets, retweets.of.retweets)
  
  # Remove the colons at the end
  retweets$retweets <- gsub(":\\b", "", retweets$retweets)
  # Remove the commas at the end
  retweets$retweets <- gsub(",\\b", "", retweets$retweets)
  # Remove the fullstops at the end
  retweets$retweets <- gsub("[.]+$", "", retweets$retweets)
  
  rts <- retweets
  
  # Find the number of occurrences of each pair
  retweets2 <- dplyr::count(retweets, user, retweets, sort = T)
  
  # Format for Gephi (Source = person who retweeted, target = person being retweeted)
  colnames(retweets2)[c(1,2,3)] <- c("Source", "Target", "Weight")
  retweets2$Type <- 'Directed'
  # retweets2$Source <- as.character(retweets$Source)  
  #  retweets2$Target <- as.character(retweets$Target)  
  
  #bad.vs<-V(g)[degree(g) < 2] 
  
  return(list(retweets2, retweets))
  # Write to files
  write.csv(retweets2,file = (debate.name), row.names=F)
}

cop21.rtwts=finding.rtsPL(pattern = "tweetsCOP21", debate.name ="COP21retweets.csv")[[1]]
write.csv(cop21.rtwts, file=paste0("./SNA/", "COP21retweets.csv"), row.names = FALSE)

cop24.rtwts=finding.rtsPL(pattern = "tweetsCOP24", debate.name ="COP24retweets.csv")[[1]]
write.csv(cop24.rtwts, file=paste0("./SNA/", "COP24retweets.csv"), row.names = FALSE)

cop25.rtwts=finding.rtsPL(pattern = "tweetsCOP25", debate.name ="COP25retweets.csv")[[1]]
write.csv(cop25.rtwts, file=paste0("./SNA/", "COP25retweets.csv"), row.names = FALSE)

cop26.rtwts=finding.rtsPL(pattern = "tweetsCOP26", debate.name ="COP26retweets.csv")[[1]]
write.csv(cop26.rtwts, file=paste0("./SNA/", "COP26retweets.csv"), row.names = FALSE)




####Corpus cleaning for Topic Model----
fireworktweets.text=read.csv("Fireworktweets.csv")
fireworkstext.forCorpus=fireworktweets.text[,c(26,4)]

colnames(fireworkstext.forCorpus)=c("doc_id", "text")
Corpus(DataframeSource(fireworkstext.forCorpus))
clean_tweet_corpus_fw <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, content_transformer(removeNumbers))
  # Remove retweets
  remove.rt <- function (x) gsub ("(RT|via)((?:\\b\\W*@\\w+)+)", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.rt))
  # Remove @ people
  remove.at.people <- function (x) gsub("@\\w+", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.at.people))
  
  remove.urls <- function (x) gsub ("http[[:alnum:]]*", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.urls))
  
  remove.weird.characters <- function (x) gsub("[^0-9A-Za-z///' ]", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.weird.characters))
  
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  
  #corpus<-tm_filter(corpus, )
  return(corpus)
}

#remove empty rows
cleaned.firecorpus=clean_tweet_corpus_fw(Corpus(DataframeSource(fireworkstext.forCorpus)))[-c(1689,70,73,82,408,839,994,1434,1473,1516,1566,1786)]
fireworks.corpus.df=data.frame(text = sapply(cleaned.firecorpus, as.character), stringsAsFactors = FALSE)

fireworks.corpus.df2= data.frame(id=sapply(cleaned.firecorpus, "content"))
data.frame(id=sapply(corpus, meta, "id"), text=unlist(lapply(sapply(corpus, [, "content"),paste,collapse="\n")), stringsAsFactors=F)

bsl.rtwts=finding.rts("BSLtweets.csv")[[1]]
write.csv(bsl.rtwts, file="BSLretweets.csv", row.names = FALSE)

bsltweets.text=read.csv("BSLtweets.csv")
bsltext.forCorpus=bsltweets.text[,c(26,4)]

colnames(bsltext.forCorpus)=c("doc_id", "text")
Corpus(DataframeSource(bsltext.forCorpus))
clean_tweet_corpus <- function(corpus){
  # Remove retweets
  remove.rt <- function (x) gsub ("(RT|via)((?:\\b\\W*@\\w+)+)", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.rt))
  # Remove @ people
  remove.at.people <- function (x) gsub("@\\w+", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.at.people))
  
  remove.urls <- function (x) gsub ("http[[:alnum:]]*", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.urls))
  
  remove.weird.characters <- function (x) gsub("[^0-9A-Za-z///' ]", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.weird.characters))
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  
  #remove.emptys <- function(x) gsub("/^$\n/", '', x)
  #corpus<- tm_map(corpus, content_transformer(remove.emptys))
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  
  #corpus<-tm_filter(corpus, )
  return(corpus)
}
cleaned.bslcorpus=clean_tweet_corpus(Corpus(DataframeSource(bsltext.forCorpus)))[-c(75,290,295,298,338,420)]
cleaned.bslcorpus=clean_tweet_corpus_fw(Corpus(DataframeSource(bsltext.forCorpus)))[-c(75,290,295,298,338,420)]
bsl.corpus.df=data.frame(text = sapply(cleaned.bslcorpus, as.character), stringsAsFactors = FALSE)

####Topic Model----
bslcorpus.dtm=DocumentTermMatrix(cleaned.bslcorpus)
rowTotals <- apply(bslcorpus.dtm , 1, sum) #Find the sum of words in each Document
bslcorpus.dtm.new   <- bslcorpus.dtm[rowTotals> 0, ]#remove empty rows
bslcorpus.10topics=LDA(bslcorpus.dtm, k=(10), control = list(seed = 1234), method="VEM")

firecorpus.dtm=DocumentTermMatrix(cleaned.firecorpus)
firecorpus.10topics=LDA(firecorpus.dtm, k=(10), control = list(seed = 1234), method="VEM")
firetopics.df=as.data.frame(topics(firecorpus.10topics,1))

firetopics.df2=data.frame(topic= topics(firecorpus.10topics,1), id=firecorpus.10topics@documents)


corpus_topics_function=function(ldafit, n){
  #tibble of words with their probability of being in each topic
  corpus_topics_function=tidytext::tidy((ldafit), matrix = "beta")
  #gets top 10 words in each topic
  corpus_top_terms_function <- corpus_topics_function %>%
    group_by(topic) %>%
    top_n((n), beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  #barplot
  corpus_top_terms_funtion_plot = corpus_top_terms_function %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  return(corpus_top_terms_funtion_plot)
}
corpus_topics_function(firecorpus.10topics,10)
dev.print(pdf, "firework_tweet_topics.pdf")

#replaces other ldavis function (uses developer version of LDAvis with reorder.topics parameter)
jsPCA <- function(phi) {
  # first, we compute a pairwise distance between topic distributions
  # using a symmetric version of KL-divergence
  # http://en.wikipedia.org/wiki/Jensen%E2%80%93Shannon_divergence
  jensenShannon <- function(x, y) {
    m <- 0.5*(x + y)
    lhs <- ifelse(x == 0, 0, x * (log(x) - log(m)))
    rhs <- ifelse(y == 0, 0, y * (log(y) - log(m)))
    0.5 * sum(lhs) + 0.5 * sum(rhs)
    #sum(ifelse(x==0,0,x * log(x/m)) + 0.5*sum(y*log(y/m)))
  }
  dist.mat <- proxy::dist(x = phi, method = jensenShannon)
  #print(as.matrix(dist.mat))
  print(which(as.matrix(dist.mat) ==Inf, arr.ind = T))
  #remove Inf values
  dist.mat[!is.finite(dist.mat)] <- 0 
  #check that they're gone - should be empty
  print(which(as.matrix(dist.mat) ==Inf, arr.ind = T))
  
  
  # then, we reduce the K by K proximity matrix down to K by 2 using PCA
  pca.fit <- stats::cmdscale(dist.mat, k = 2)
  data.frame(x = pca.fit[,1], y = pca.fit[,2])
}
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE),
    mds.method = jsPCA, reorder.topics = FALSE
  )
}
serVis(topicmodels2LDAvis(firecorpus.10topics))
terms(firecorpus.10topics,5)


serVis(topicmodels2LDAvis(bslcorpus.10topics))
terms(bslcorpus.10topics,5)

# jsonfire.lda10=topicmodels_json_ldavis(firecorpus.10topics, cleaned.firecorpus,firecorpus.dtm)
# serVis(jsonfire.lda10)

#30 topics didn't change much so sticking with 10
# firecorpus.30topics=LDA(firecorpus.dtm, k=(30), control = list(seed = 1234), method="VEM")
# jsonfire.lda30=topicmodels_json_ldavis(firecorpus.30topics, cleaned.firecorpus,firecorpus.dtm)
# serVis(jsonfire.lda30)




# m <- as.matrix(firecorpus.dtm)
# v <- sort(colSums(m), decreasing=TRUE)
# head(v, 10)

####Bigrams----

#from molly
library(igraph)
create.ngram <- function (input.df, ngram, nodes.to.remove ) {
  
  # Extract a list containg each chunk of 5 words
  edgelist.5gram <- input.df %>%
    unnest_tokens(bigram, text, token = "ngrams", n= 5) ##  
  
  result.vec <- table(unlist(lapply(edgelist.5gram$bigram, function(bigram) {
    pairs <- combn(unique(scan(text=bigram, what='', sep=' ')), m=2)
    interaction(pairs[1,], pairs[2,])
  })))
  
  # Convert to a dataframe and process so directionality in bigrams does not matter
  result.vec.df <- as.data.frame(result.vec)
  result.vec.df$Var1 <- as.character(result.vec.df$Var1) 
  result.vec.df$Var1 <- gsub("\\.", " ", result.vec.df$Var1) 
  result.vec.df$sorted <- sapply(result.vec.df$Var1,function(x)paste0(sort(unlist(strsplit(x," ")),decreasing=F),collapse=" "))
  result.vec.df <- result.vec.df[c(3,2)]
  weighted.edges <- result.vec.df %>%
    count(sorted, vars = as.numeric(result.vec.df$Freq), sort = TRUE)
  weighted.edges <- weighted.edges[c(1,2)]
  
  # Split bigrams into two columns
  split.figrams <- strsplit(as.character(weighted.edges$sorted),' ') 
  weighted.edges.1 <- data.frame(do.call(rbind, split.figrams), weighted.edges$vars)
  weighted.edges.1 <- subset(weighted.edges.1, weighted.edges.vars > 0)
  
  # Create a graph object from the edgelist and then convert back to an adjacency matrix
  g <- graph.data.frame(weighted.edges.1, directed=FALSE);
  adj <- get.adjacency(g, type="both",names=TRUE, sparse=FALSE,attr= "weighted.edges.vars")
  
  # CReate a graph from the adjacency matrix
  g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
  
  # Simplify by removing loops and repeated values
  g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)  
  
  #remove.edges <- "yes"
  # Define cutoff level based on mean
  cut.off <- mean(E(g)$weight)
  #Cut off edges for those lower than the mean (for those defined as 'yes' in function input)
  # g <- if(remove.edges == 'yes') {
  g <- delete_edges(g, E(g)[weight< 5]) #cutoff
  #} else {
  #  g}
  
  g=delete.vertices(g,which(degree(g)<1))
  
  # Better to remove isolated nodes than to delete edges with a low weight as this results in more isolated
  # nodes creating a dispersed graph
  
  # Convert to a dataframe for use in Gephi and rename columns accordingly
  df <- get.data.frame(g)
  colnames (df)[c(1,2,3)] <- c("Source","Target", "Weight" )
  df$Type <- 'Undirected'
  
  # Find the list of nodes and their number of occurrences.
  # Method involves finding the number of nodes and their occurences from the start (extracted from the df)
  # Then, calculating from the bigram edgelist from the end the words that are remaining
  # and joining these to the table from the start to see how many times they occurred
  # The earlier removal of bad.vs doesn't make this incorrect as it removes individual nodes rather than edges between
  # them and so the count of occurrences should remain accurate
  # Nb. In some instances the count of the appearances of a word in a bigram may be greater than the count of its appearances
  # alone. This occurs in situations, for instance where the tweet reads "twitter android twitter" and so the bigram "android twitter"
  # has two occurrences despite the word only occurring once. 
  edgelist <- input.df %>%
    unnest_tokens(word, text)
  word.occurences <- edgelist %>%
    count(word, sort = TRUE)
  # Find nodes from the end
  unique1 <- data.frame(word = unique (df$Source))
  unique2 <- data.frame(word = unique (df$Target))
  together <- rbind(unique1, unique2)
  together <- data.frame(word = unique (together$word))
  word.occurences <- inner_join(together, word.occurences, by = 'word')
  
  # Find the communities hidden within the graph
  find.community <- function (input.graph){
    g <- input.graph
    # Descriptors of algorithms found here: http://stackoverflow.com/questions/9471906/what-are-the-differences-between-community-detection-algorithms-in-igraph
    ## Modularity measures This metrics measures the strength of division of a network into modules. 
    # Networks with high modularity have dense connections between the nodes within modules but sparse 
    # connections between nodes in different modules.
    # Modularity is basically the fraction of the edges that fall within the given groups minus the expected
    # such fraction if edges were distributed at random. So the higher the better.
    #  scores <- data.frame(
    #    wc = modularity (walktrap.community(g)),
    #   fg = modularity(fastgreedy.community(g)),
    #    lp = modularity(label.propagation.community(g)),
    #    eb = modularity(edge.betweenness.community(g)),
    #    sl = modularity(spinglass.community(g))
    #   # ,le = modularity(leading.eigenvector.community(g))
    #  )
    
    # Find the algorithm with the highest score
    #  winner <- as.character(colnames(scores)[apply(scores,1,which.max)])
    
    # Use this output to set a value for wt for later use
    #  community = if (winner == "fg") fastgreedy.community(g) else 
    #    if (winner == "sl") spinglass.community (g) else 
    #      if (winner == "wc") walktrap.community (g) else  
    #        if (winner == "lp") label.propagation.community (g) else 
    #          if (winner == "eb") edge.betweenness.community (g) 
    #          #  if (winner == "le") leading.eigenvector.community (g, options = list(maxiter= 10000000))
    #              else "" 
    community =  fastgreedy.community(g)
  }
  community <- find.community(g)
  community.membership <- data.frame(nodes = community$names,
                                     cluster.number = community$membership)
  colnames(community.membership)[1] <- "ID"
  
  return(list(word.occurences, df, community.membership))
}

gramtest <- create.ngram(bsl.corpus.df, 5, 10)[[1]]
gramtest.5gram <- create.ngram(bsl.corpus.df, 5, 10)[[2]]

write.csv(gramtest.5gram, file="BSL5grams.csv", row.names = FALSE)

firework.tweet.5gram <- create.ngram(fireworks.corpus.df, 5, 10)[[2]]
get_5gram_net=function(ngram.df, n, debate){
    ngram.df %>%
    filter(Weight >= (n)) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = Weight)) +
    #scale_edge_width(range = 10) +
    geom_node_point(color = "black", size = 2) +
    #geom_node_circle(aes(r=0.2))+
    geom_node_text(aes(label = name), vjust = -0.2, hjust=-0.2,check_overlap = TRUE ,repel = TRUE) +
    ggtitle(paste(debate,":", "n =", n, sep = " "))+
    theme_void()
}

get_5gram_net(gramtest.5gram, 100, "BSL Debate")
get_5gram_net(firework.tweet.5gram, 300, "Firework Tweet Debate")
####Sentiment Analysis----
#make into 1-word-per-row format
firetweets.forSenti=fireworks.corpus.df%>% unnest_tokens(word, text)
bsltweets.forSenti=bsl.corpus.df%>% unnest_tokens(word, text)


#then use functions from Sentiment Analysis.R 

#plotting distribution for afinn
plot_senti_dist_afinn= function(tibble, debate){
  get_sentiment_frequencies_afinn = function(tibble){
    tibble %>%
      inner_join(get_sentiments("afinn")) %>%
      count(word,score, sort = TRUE) %>%
      ungroup()
  }
  afinn_frequencies=get_sentiment_frequencies_afinn(tibble)
  ggplot(data= as.data.frame(table(afinn_frequencies$score)/nrow(afinn_frequencies)*100), aes(x=Var1, y=Freq, fill=as.numeric(Var1))) + 
    geom_bar(stat='identity') +
    ylab("Percentage of words") +
    xlab("Sentiment category") +
    ggtitle(paste(debate,":", "Afinn Sentiment Distribution", sep = " ")) +
    theme(legend.position = "none")
  
}
plot_senti_dist_afinn(firetweets.forSenti, "Fireworks Debate")

plot_senti_dist_afinn(bsltweets.forSenti, "BSL Debate")

#plotting distribution for others
plot_senti_dist= function(tibble, lexicon, debate){
  get_sentiment_frequencies = function(tibble, lexicon){
    tibble %>%
      inner_join(get_sentiments(lexicon)) %>%
      count(word,sentiment, sort = TRUE) %>%
      ungroup()
  }
  senti_frequencies=get_sentiment_frequencies(tibble, lexicon)
  
  ggplot(data= as.data.frame(table(senti_frequencies$sentiment)/nrow(senti_frequencies)*100), aes(x=Var1, y=Freq, fill=Var1)) + 
    geom_bar(stat='identity') +
    ylab("Percentage of words") +
    xlab("Sentiment category") +
    ggtitle(paste(debate,":", lexicon,"Sentiment Distribution", sep=" ")) +
    theme(legend.position = "none")
  
}
plot_senti_dist(firetweets.forSenti, "nrc", "Fireworks Debate")
plot_senti_dist(bsltweets.forSenti, "nrc", "BSL Debate")

plot_senti_dist(firetweets.forSenti, "bing", "Fireworks Debate")
plot_senti_dist(bsltweets.forSenti, "bing", "BSL Debate")

#plotting the words
plot_sentiments(get_sentiment_frequencies(firetweets.forSenti, "bing"), 'Firework Debate')
plot_sentiments(get_sentiment_frequencies(bsltweets.forSenti, "bing"), 'BSL Debate')

plot_sentiments(get_sentiment_frequencies(firetweets.forSenti, "nrc"), 'Firework Debate')
plot_sentiments(get_sentiment_frequencies(bsltweets.forSenti, "nrc"), 'BSL Debate')

plot_sentiments_afinn(get_sentiment_frequencies_afinn(firetweets.forSenti), "Firework Debate")
plot_sentiments_afinn(get_sentiment_frequencies_afinn(bsltweets.forSenti), "BSL Debate")

##Sentiment plotting and saving----
library(gridExtra)

grid.arrange(plot_senti_dist(bsltweets.forSenti, "nrc", "BSL Debate"), plot_sentiments(get_sentiment_frequencies(bsltweets.forSenti, "nrc"), 'BSL Debate'), ncol = 1, nrow=2, heights=2:3)
dev.print(pdf, "bsl_tweet_nrc_.pdf")

grid.arrange(plot_senti_dist(bsltweets.forSenti, "bing", "BSL Debate"), plot_sentiments(get_sentiment_frequencies(bsltweets.forSenti, "bing"), 'BSL Debate'), ncol = 1, nrow=2, heights=2:3)
dev.print(pdf, "bsl_tweet_bing_.pdf")

grid.arrange(plot_senti_dist_afinn(bsltweets.forSenti, "BSL Debate"), plot_sentiments_afinn(get_sentiment_frequencies_afinn(bsltweets.forSenti), "BSL Debate"), ncol = 1, nrow=2, heights=2:3)
dev.print(pdf, "bsl_tweet_afinn_.pdf")

grid.arrange(plot_senti_dist(firetweets.forSenti, "nrc", "Fireworks Debate"), plot_sentiments(get_sentiment_frequencies(firetweets.forSenti, "nrc"), 'Fireworks Debate'), ncol = 1, nrow=2, heights=2:3)
dev.print(pdf, "fireworks_tweet_nrc_.pdf")

grid.arrange(plot_senti_dist(firetweets.forSenti, "bing", "Fireworks Debate"), plot_sentiments(get_sentiment_frequencies(firetweets.forSenti, "bing"), 'Fireworks Debate'), ncol = 1, nrow=2, heights=2:3)
dev.print(pdf, "fireworks_tweet_bing_.pdf")

grid.arrange(plot_senti_dist_afinn(firetweets.forSenti, "Fireworks Debate"), plot_sentiments_afinn(get_sentiment_frequencies_afinn(firetweets.forSenti), "Firework Debate"), ncol = 1, nrow=2, heights=2:3)
dev.print(pdf, "fireworks_tweet_afinn_.pdf")

#Debugging corpus cleaning----
# #debugging corpus creation - order of tm_maps matter!
# inspect(DocumentTermMatrix(VCorpus(DataframeSource(fireworkstext.forCorpus)) 
#                            %>% tm_map(content_transformer(remove.rt))
#                            %>% tm_map(content_transformer(remove.at.people))
#                            %>% tm_map(content_transformer(remove.urls))
#                            %>% tm_map(content_transformer(remove.weird.characters))
#                            %>% tm_map(removePunctuation) 
#                            %>% tm_map(content_transformer(tolower)) 
#                            %>% tm_map(removeWords, stopwords("en"))
#                            %>% tm_map(content_transformer(stripWhitespace))
#                            %>% tm_map(content_transformer(removeNumbers))
# 
#                            )
#         )

