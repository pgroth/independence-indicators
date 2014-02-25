# Parse paper .csv files from Google Scholar citation data
#
# Anca Dumitrache
# 02/12/2012

library(igraph)

source('indicatorGraphCreation.R')

get_publications <- function(data) {
	pubs <- vector(mode = "list", length = length(data$Authors))
	for (i in 1:length(data$Authors)) {
		auth <- strsplit(toString(data$Authors[i]), "; ")
		authors <- c()
		for (j in 1:length(auth[[1]])) {
      str <- auth[[1]][j]
			authors <- append(authors, tolower(str))
		}
		pubs[[i]] <- list(authors = authors, title = toString(data$Title[i]))
	}
	return(pubs)
}

parseGoogleScholar <- function(researcher, rfile, supervisor, sfile) {
  data_researcher <- get_publications(read.csv(rfile, head=TRUE, sep=","))
  data_supervisor <- get_publications(read.csv(sfile, head=TRUE, sep=","))
  
  coauthor_graph <- get_coauthor_graph(data_researcher, tolower(researcher), tolower(supervisor), weighted=FALSE)
  title_sim_graph <- get_title_sim_graph(data_researcher, data_supervisor, researcher, supervisor)
  
  #plot(title_sim_graph)
  #title(main="Title similarity in the combined network of paper titles")
  
  # write graph to file
  png(filename="../img/paper_coauthor_network.png")
  plot(coauthor_graph)
  title(main=paste("Co-author network", researcher, sep=" "))
  dev.off()
  
  # write graph to file
  png(filename="../img/paper_title_similarity.png")
  plot(title_sim_graph)
  title(main="Title similarity in the combined network")
  dev.off()
  
  # write graphs to file
  write.graph(title_sim_graph, "../gephi/paper_titles_graph.gml", "gml")
  write.graph(coauthor_graph, "../gephi/paper_coauthor_netw_graph.gml", "gml")
}
