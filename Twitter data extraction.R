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

