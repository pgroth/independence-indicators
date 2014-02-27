# Retrieve WikiCFP data
#
# Anca Dumitrache

library(RCurl)
library(XML)
library(rjson)
library(stringr)

source('indicatorGraphCreation.R')

get_cfps <- function(NAME) {
  name <- gsub(" ", "+", NAME)
  url <- paste("http://www.google.com/search?q=site%3Ahttp%3A%2F%2Fwww.wikicfp.com%2Fcfp%2F+%22Paul+Groth%22&aq=f&oq=site%3Ahttp%3A%2F%2Fwww.wikicfp.com%2Fcfp%2F+%22",paste(name,"%22", sep=""), sep="")
  
  source("crawler.R")
  dom <- get(url)
  
  res <- get_elt(dom,'//div[@id="resultStats"]//text()')
  number_res <- as.numeric(gsub(" ", "", gsub("[a-z]*[A-Z]*","", res)))
  
  links <- get_first_attr(dom,paste('//h3[@class="r"]/a/@href',sep=''))
  
  start <- 10
  while (start < number_res) {
    new_url <- paste(url, "&start=", start, sep="")
    dom <- get(new_url)
    links <- append(links, get_first_attr(dom,paste('//h3[@class="r"]/a/@href',sep='')))
    
    start <- start + 10
  }
  
  for (i in 1:length(links)) {
    links[i] <- gsub("%3F","?",links[i])
    links[i] <- gsub("%3D","=",links[i])
    links[i] <- gsub("%26","&",links[i])
    links[i] <- substring(links[i], 8)
  }
  
  authors <- list()
  titles <- list()
  tags <- list()
  cfps <- vector(mode = "list", length = length(links))
  
  for (i in 1:length(links)) {
    dom<-get(links[i])
    title <- get_elt(dom,'//title//text()')
    #print(title)
    titles[[length(titles) + 1]] <- title
    
    start_date <- xpathSApply(dom, '//span[@typeof="v:Event"]/span[@property="v:startDate"]', function(x) xmlAttrs(x)[["content"]])[1]
    # print(start_date)
    end_date <- xpathSApply(dom, '//span[@typeof="v:Event"]/span[@property="v:endDate"]', function(x) xmlAttrs(x)[["content"]])[1]
    # print(end_date)
    location <- xpathSApply(dom, '//span[@typeof="v:Address"]/span[@property="v:locality"]', function(x) xmlAttrs(x)[["content"]])[1]
    # print(location)
    
    text <- xpathSApply(dom, '//td[@align="center"]/div[@class="cfp"]/text()', xmlValue)
    #print(text)
    a <- c()
    parsing_orgs = FALSE
    cons <- 0
    for (j in 1:length(text)) {
      str <- tolower(text[j])
      if (parsing_orgs == TRUE 
          && str_detect(str,"chairs") == FALSE 
          && str_detect(str,"committee") == FALSE
          && str_detect(str,"organizers") == FALSE) {
        #print(str)
        if (str_detect(str,",")) {
          auth = str_split_fixed(str,",",100)[1]
          auth = str_trim(str_replace_all(auth, "[^a-z^A-Z ]", ""))
          # print(auth)
          a[length(a) + 1] <- auth
          cons <- cons + 1
        }
        else {
          if (cons > 0 && str_replace_all(str, "[^a-z^A-Z]", "") == "") {
            cons <- 0
            parsing_orgs = FALSE
          }
        }
      }
      if (str_detect(str,"chairs")
          || str_detect(str,"committee")
          || str_detect(str,"organizers")) {
        parsing_orgs = TRUE
        cons <- 0
      }
    }
    authors[[length(authors) + 1]] <- a
    
    cats <- xpathSApply(dom, '//h5[a[@href="/cfp/allcat"]]/a', xmlValue)
    if (length(cats) > 0) {
      tags[[length(tags) + 1]] <- cats[-1]
    }
    else {
      tags[[length(tags) + 1]] <- c("NULL")
      cats <- c("NULL")
    }
    
    cfps[[i]] <- list(authors=a, tags=cats[-1], title=title, url=links[i])
  }
  
  return(cfps)
}

parseWikiCFP <- function(researcher, supervisor) {
  cfps_researcher <- get_cfps(researcher)
  cfps_supervisor <- get_cfps(supervisor)
  
  coauthor_graph <- get_coauthor_graph(cfps_researcher, researcher, supervisor,cfps=TRUE)
  tag_sim_graph <- get_title_sim_graph(cfps_researcher, cfps_supervisor, researcher, supervisor,cfps=TRUE)
  
  #plot(tag_sim_graph)
  #title(main="Similarity in the combined network of conference tags")
  
  # save coauthor network picture
  png(filename="../img/cfp_coauth_network.png")
  plot(coauthor_graph)
  title(main=paste("CFP co-author network", researcher, sep=" "))
  dev.off()
  
  # save tag similarity picture
  png(filename="../img/cfp_tag_similarity.png")
  plot(tag_sim_graph)
  title(main="Similarity in the combined network of conference tags")
  dev.off()
  
  # write graphs to file
  write.graph(tag_sim_graph, "../gephi/cfp_tags_graph.gml", "gml")
  write.graph(coauthor_graph, "../gephi/cfp_coauthor_netw_graph.gml", "gml")
}

