library(RCurl)
library(XML)
library(igraph)
library(RecordLinkage)

get_and_xpath <- function(URL,XPath) {
  dom <- get(URL)
  xpath(dom,XPath)
}

get <- function(URL) {
  # print(paste("Getting",URL))
  htmlParse(getURL(URL,followlocation = TRUE, httpheader=c("User-Agent" = "Harmless Crawler, <rinke.hoekstra@vu.nl>")))
}

get_all <- function(URLs) {
  doms <- c()
  for(URL in URLs) {
    dom <- get(URL)
    doms <- append(doms,dom)
  }
  names(doms) <- unlist(URLs)
  return(doms)
}

xpath <- function(dom,XPath) {
  getNodeSet(dom,XPath)
}

get_attrs <- function(dom,XPath) {
  sapply(xpath(dom,XPath),xmlAttrs)
}

get_first_attr <- function(dom,XPath) {
  sapply(xpath(dom,XPath),function(x){x[[1]]})
}

get_elt <- function(dom,XPath) {
  sapply(xpath(dom,XPath),xmlValue)
}

get_wiki_lang_link <- function(dom,lang) {
  get_first_attr(dom,paste('//li[@class="interwiki-',lang,'"]/a/@href',sep=''))
}

get_wiki_body_content <- function(dom) {
  get_elt(dom,'//div[@id="bodyContent"]//*[not(child::p)]//text()')
}



get_wiki_links <- function(URL,dom) {
  # Get all 'html:a' elements from the bodyContent div 
  anchors <- get_attrs(dom,'//div[@id="main-content"]//a[not(contains(@class,"nofollow")) and not(contains(@class,"extiw"))]')
  # Get all only those anchors for which the href attribute does not point to an internal 
  # MediaWiki page or category page.
  wiki_anchors <- anchors[which(sapply(anchors, function(x) {grepl("/wiki/[[:alnum:]_]+$",x['href'])}))]
  wiki_links <- sapply(wiki_anchors, function(x) { absolute_wiki_url(x['href'],URL) })
  return(wiki_links)
}

get_profile_links <- function(URL) {
  dom <- get(URL)
  # Get all 'html:a' elements from the bodyContent div 
  anchors <- get_attrs(dom,'//div[@id="search_results"]//a[not(contains(@class,"nofollow")) and not(contains(@class,"extiw"))]')
  profile_anchors <- anchors[which(sapply(anchors, function(x) {grep("/profiles/",x['href'])}) == 1)]
  profile_links <- sapply(profile_anchors, function(x) { absolute_wiki_url(x['href'],URL) })
  return(unique(profile_links))
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

get_publications <- function(URL) {
  dom <- get(URL)
  print(paste("following",URL))
  anchors <- get_elt(dom,'//div[@class="document-desc "]/node()[not(self::a) and not(self::em) and not(self::div) and not(self::span)]')

  authors <- list()
  asize <- 1
  sapply(anchors, function(x) {
    a <- trim(x)
    if (a != "" && substr(a,1,1) != "," && substr(a,1,1) != ".") {
      asplit <- strsplit(a, "\\,")
      alist <- list()
      for (i in 1:length(asplit[[1]])) {
        author <- trim(asplit[[1]][i])
        author <- gsub("[^[:alpha:] ]", "", author)

	alist <- append(alist, author)
      }
      authors[[asize]] <<- alist
      asize <<- asize + 1
    }
  })

  papers <- get_elt(dom,'//div[@class="document-desc "]/a')

  pubs <- vector(mode = "list", length = length(authors))
  for (i in 1:length(authors)) {
    pubs[[i]] <- list(authors = authors[[i]], paper = papers[i])
  }
  
  return(pubs)
}

absolute_wiki_url <- function(URL,cur_page) {
  sp <- strsplit(cur_page,'/')[[1]]
  if(substr(URL,1,2) == '//') {
    paste('http:',URL,sep="")
  } else if(substr(URL,1,1) == '/') {
    paste('http://',sp[3],URL,sep="")
  } else if(substr(URL,1,1) == '#') {
    paste(cur_page,URL,sep="")
  } else {
    URL
  }
}

politicus_filter <- function(dom) {
  if (length(xpath(dom,'//div[@id="mw-normal-catlinks"]//a[contains(./text(),"politicus")]')) > 0) {
    TRUE
  } else {
    FALSE
  }  
}

politicus_of_kabinet_filter <- function(dom) {
  if (length(xpath(dom,'//div[@id="mw-normal-catlinks"]//a[contains(./text(),"politicus")]')) > 0 || length(xpath(dom,'//div[@id="mw-normal-catlinks"]//a[contains(./text(),"Nederlands kabinet")]')) > 0) {
    TRUE
  } else {
    FALSE
  }
}

living_people_filter <- function(dom) {
  if (length(xpath(dom,'//a[./text()="Living people"]')) > 0) {
    TRUE
  } else {
    FALSE
  }
}

living_people_or_movie_filter <- function(dom) {
  if (length(xpath(dom,'//div[@id="mw-normal-catlinks"]//a[./text()="Living people"]')) > 0 || length(xpath(dom,'//div[@id="mw-normal-catlinks"]//a[contains(./text(),"movie")]')) > 0) {
    TRUE
  } else {
    FALSE
  }
}

movie_filter <- function(dom) {
  if (length(xpath(dom,'//div[@id="mw-normal-catlinks"]//a[contains(./text(),"movie")]')) > 0) {
    TRUE
  } else {
    FALSE
  }
}

crawl_depth_first_aux <- function(URL,step_limit,selection,done) {
  if (step_limit < 1) {
    return(list(edges=c(),done=done))
  } else {
    if (!(URL %in% done)) {
      done <- unique(append(done,c(URL)))
      try( {
          dom <- get(URL)
          if (!is.null(selection) && !sapply(c(dom),selection)) {
            print(paste("not following",URL))
            return(list(edges=c(),done=done))
          } else {
            print(paste("following",URL))
            URL_name <- gsub('http://.*/','',URL)
            links <- unique(get_profile_links(URL,dom))
            edges <- c()
            for (l in links) {
              l_name <- l
              edges <- append(edges,c(URL_name,l_name))
              results <- crawl_depth_first_aux(l,step_limit - 1,selection,done)
              edges <- append(edges,results$edges)
              done <- unique(append(done,results$done))
            }
            return(list(edges=edges,done=done))
          }
        }, silent=FALSE)
    } else {
      print(paste("already visited",URL))
    }
    return(list(edges=c(),done=done))
  }
}

crawl <- function(URL,step_limit,selection=NULL) {
  graph.edgelist(t(matrix(crawl_depth_first_aux(URL,step_limit,selection,done=c())$edges,nrow=2)))
}

get_all_papers_type <- function(URL, type) {
  more_papers <- TRUE
  papers <- list()
  i <- 0
  while (more_papers == TRUE) {
    np <- get_publications(paste(URL, paste("publications",paste(type,as.character(i),sep="/#"), sep="/"), sep="/"))
    if (length(papers) != 0 && np[[length(np)]]$paper == papers[[length(papers)]]$paper) {
      more_papers <- FALSE
    }
    else {
      papers <- append(papers, np)
      i <- i + 1
    }
  }
  return(papers)
}

get_all_papers <- function(URL) {
  papers <- list()
  papers <- append(papers, get_all_papers_type(URL, "journal"))
  papers <- append(papers, get_all_papers_type(URL, "report"))
  papers <- append(papers, get_all_papers_type(URL, "thesis"))
  papers <- append(papers, get_all_papers_type(URL, "book"))
  papers <- append(papers, get_all_papers_type(URL, "book_section"))
  papers <- append(papers, get_all_papers_type(URL, "conference_proceedings"))
  papers <- append(papers, get_all_papers_type(URL, "generic"))

  # check for other spellings of the same name
  all_authors <- list()
  x <- 1
  y <- 1
  sapply(papers, function(pap) {
    y <<- 1
    sapply(pap$authors, function(author) {
	found_auth <- ""
        if (length(all_authors) != 0 && length(author) != 0) {
		for (j in 1:length(all_authors)) {
		  if (levenshteinDist(all_authors[[j]], author) + min(nchar(all_authors[[j]]),nchar(author)) < max(nchar(all_authors[[j]]),nchar(author)) + 3) {
		    found_auth <- all_authors[[j]]
		  } 
		}
        }
	if (found_auth != "") {
	  author <<- found_auth	
          papers[[x]]$authors[[y]] <<- found_auth
	}
	else {
	  all_authors[[length(all_authors) + 1]] <<- author
	}
        y <<- y + 1
    })
    x <<- x + 1
  })
  print(all_authors)

  return(papers)
}

save.graph <- function(g, filename) {
  edgelist <- get.edgelist(g)
  dimnames(edgelist) <- list(c(),c("Source","Target"))
  print(paste("Saving to: ",filename,".csv",sep=""))
  write.csv(edgelist, file=paste(filename,".csv",sep=""))
  print("Done")
}


