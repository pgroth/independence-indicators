# functions for building the co-author and topic/title similarity graphs
#
# Anca Dumitrache

library(igraph)

# Adapted from R-Help:
#    G. Jay Kerns
#    Dec 22, 2007; 11:55pm
#    Re: number of count of each unique row
# Returns a data.frame of unique edges and counts
edge.count3 <- function(g){
  D <- data.frame(get.edgelist(g))  # convert to data frame
  ones <- rep(1, nrow(D))   # a column of 1s
  result <- aggregate(ones, by = as.list(D), FUN = sum)
  names(result) <- c("from", "to", "count")
  result
}

get_coauthor_graph <- function(paper_list, researcher, supervisor, weighted=FALSE, cfps=FALSE) {
  # get coauthors of young researcher
  coauthors_all <- c()
  coauthors_per_paper <- list()
  edgelist <- c()
  papersWithSupervisor <- 0
  papersPerAuthor <- list()
  
  lapply(paper_list, function(i) {
    coauthors_per_paper <<- list()
    lapply(i$authors, function(name) {
      if (tolower(name) != tolower(researcher)) {
        coauthors_all[[length(coauthors_all) + 1]] <<- tolower(name)
        coauthors_per_paper[[length(coauthors_per_paper) + 1]] <<- tolower(name)
        
        if ((tolower(name) %in% names(papersPerAuthor)) == FALSE) {
          papersPerAuthor[[tolower(name)]] <<- 1
        }
        else {
          papersPerAuthor[[tolower(name)]] <<- papersPerAuthor[[tolower(name)]] + 1
        }
        
        if (tolower(name) == tolower(supervisor)) {
          papersWithSupervisor <<- papersWithSupervisor + 1
        }
      }
    })
    if (length(coauthors_per_paper) != 0) {
      for (i in 1:length(coauthors_per_paper)) {
        edgelist <<- append(edgelist, tolower(researcher))
        edgelist <<- append(edgelist, coauthors_per_paper[[i]])
        if (i != length(coauthors_per_paper)) {
          for (j in (i+1):length(coauthors_per_paper)) {
            edgelist <<- append(edgelist, coauthors_per_paper[[i]])
            edgelist <<- append(edgelist, coauthors_per_paper[[j]])
          }
        }
      }
    }	
  })
  coauthors_all <- unique(coauthors_all)
  
  # create researcher co-author graph
  el <- matrix(edgelist, nc=2, byrow=TRUE)
  if (weighted == TRUE) {		
    coauthor_graph <- as.undirected(graph.edgelist(el), "collapse")
  }
  else {
    coauthor_graph <- as.undirected(graph.edgelist(el), "collapse")
  }
  
  V(coauthor_graph)[tolower(researcher)]$color <- "green"
  V(coauthor_graph)[tolower(supervisor)]$color <- "red"
  
  coauthor_graph_edge_count <- edge.count3(coauthor_graph)
  vec <- as.vector(coauthor_graph_edge_count["count"][[1]])
  E(coauthor_graph)$weight <- vec
  #plot(coauthor_graph)
  #title(main=paste("Ego network", researcher, sep=" "))
  
  # papers with former supervisor
  str = "PAPERS WITH FORMER SUPERVISOR (%):"
  if (cfps == TRUE) str = "CFPS WITH FORMER SUPERVISOR (%):"
  print(paste(str,
              papersWithSupervisor * 100 / length(paper_list),
              sep=" "))
  
  # number of coauthors
  str = "NUMBER OF COAUTHORS:"
  if (cfps == TRUE) str = "CFP NUMBER OF COAUTHORS:"
  print(paste(str,
              length(coauthors_all),
              sep=" "))
  
  # median number of relations with co-author 
  str = "MEDIAN NUMBER OF RELATIONS WITH COAUTHOR"
  if (cfps == TRUE) str = "CFP MEDIAN NUMBER OF RELATIONS WITH COAUTHOR:"
  print(paste(str,
        mean(unlist(lapply(papersPerAuthor, function(z) z))),
        sep=" "))
  
  # eigenvector centrality
  eigenvector_centrality <- evcent(coauthor_graph, directed=FALSE, scale=TRUE)
  str = "EIGENVECTOR CENTRALITY YOUNG RESEARCHER:"
  if (cfps == TRUE) str= "CFP EIGENVECTOR CENTRALITY YOUNG RESEARCHER:"
  print(paste(str,
        eigenvector_centrality$vector[[tolower(researcher)]],
        sep=" "))
  
  str = "EIGENVECTOR CENTRALITY SUPERVISOR:"
  if (cfps == TRUE) str = "CFP EIGENVECTOR CENTRALITY SUPERVISOR:"
  print(paste(str,
        eigenvector_centrality$vector[[tolower(supervisor)]],
        sep=" "))
  
  index <- 0
  for (i in 1:length(coauthors_all)) {
    if (coauthors_all[i] == tolower(supervisor)) {
      index <- i+1
    }
  }
  
  # clustering coefficient
  str = "CLUSTERING COEFFICIENT:"
  if (cfps == TRUE) str = "CFP CLUSTERING COEFFICIENT:"
  print(paste(str,
              transitivity(coauthor_graph, vids=c(1,index), type="local", weights=vec),
              sep=" "))
  
  return(coauthor_graph)
}

get_tags <- function(conf1, conf2, title) {
  for (i in 1:length(conf1))
    if (title == conf1[[i]]$title)
      return(conf1[[i]]$tags)
  for (i in 1:length(conf2))
    if (title == conf2[[i]]$title)
      return(conf2[[i]]$tags)
  return(c())
}

get_title_sim_graph <- function(papersA, papersB, researcher, supervisor, cfps=FALSE) {
  source('titles.R')
  
  paper_edges <- c()
  paper_set <- c()
  weights <- c()
  
  for (i in 1:length(papersA)) {
    aux <- c(papersA[[i]]$title)
    paper_set <- union(paper_set, aux)
  }
  for (i in 1:length(papersB)) {
    aux <- c(papersB[[i]]$title)
    paper_set <- union(paper_set, aux)
  }
  
  w <- 0
  for (i in 1:(length(paper_set) - 1)) {
    for (j in (i + 1):length(paper_set)) {
      
      if (cfps == TRUE) {
        tagsi <- get_tags(papersA,papersB,paper_set[i])
        tagsj <- get_tags(papersA,papersB,paper_set[j])
        if (length(tagsi) != 0 && length(tagsj) != 0) {
          w <- length(intersect(tagsi,tagsj)) / length(union(tagsi,tagsj))
        }
      }
      else
        w <- compute_similarity(paper_set[i], paper_set[j])
      if (w > 0.2) {
        paper_edges <- append(paper_edges, paper_set[i])
        paper_edges <- append(paper_edges, paper_set[j])
        weights <- append(weights, w)
      }
    }
  }
  
  el <- matrix(paper_edges, nc=2, byrow=TRUE)
  pe <- as.undirected(graph.edgelist(el), "each")
  
  single_nodes <- 0
  
  for (i in 1:length(papersA)) {
    if (papersA[[i]]$title %in% V(pe)$name) {
      if (tolower(supervisor) %in% papersA[[i]]$authors == FALSE) {
        V(pe)[(papersA[[i]]$title)]$color <- "green"
      }
      else {
        V(pe)[(papersA[[i]]$title)]$color <- "red"
      }	
    }
    else {
      single_nodes <- single_nodes + 1
    }
  }
  
  print(paste("SINGLE NODES IN SIMILARITY GRAPH:", single_nodes, sep = " "))
  print("")
  
  for (i in 1:length(papersB)) {	
    if (papersB[[i]]$title %in% V(pe)$name) {
      if (tolower(researcher) %in% papersB[[i]]$authors == FALSE) {
        V(pe)[papersB[[i]]$title]$color <- "blue"
        
      }
      else {
        V(pe)[papersB[[i]]$title]$color <- "red"
      }
    }
  }
  E(pe)$weight <- weights
  return(pe)
}

