# Computes paper title similarity
#
# Anca Dumitrache
# 02/12/2012

library(tm)
library(wordnet)
library(openNLP)

synonym_limit <- 5
# setDict("C:/Program Files (x86)/WordNet")

synonyms_cache <- list()

cleanup_title <- function(paper) {
  # paper <- removePunctuation(paper)
  paper <- gsub("[[:punct:]]", " ", paper)
  paper <- removeNumbers(paper)
  paper <- removeWords(paper, stopwords("english"))
  paper <- stemDocument(paper)
  paper <- stripWhitespace(paper)
  return(paper)
}

get_synonyms <- function(paper) {	
  if (!is.null(synonyms_cache[[paper]])) {
  	return(synonyms_cache[[paper]])
  }
  
  old_paper <- paper
	paper <- cleanup_title(paper)
	
	
	word_list <- strsplit(paper, " ")
	syn_list <- list()
	
	for (i in 1:length(word_list[[1]])) {
		word <- word_list[[1]][i]
				
		if (word != "" && nchar(word) > 2) {
			 # {
				synonyms <- c(word)
				
				# filter <- getTermFilter("ContainsFilter", word, TRUE)
			
				# terms <- getIndexTerms("NOUN", synonym_limit, filter)
				# if (length(terms) > 0) {
				# 	synonyms <- append(synonyms, getSynonyms(terms[[1]]))
				# }
				# terms <- getIndexTerms("ADJECTIVE", synonym_limit, filter)
				# if (length(terms) > 0) {
				# 	synonyms <- append(synonyms, getSynonyms(terms[[1]]))
				# }
				# terms <- getIndexTerms("ADVERB", synonym_limit, filter)
				# if (length(terms) > 0) {
				# 	synonyms <- append(synonyms, getSynonyms(terms[[1]]))
				# }
				# terms <- getIndexTerms("VERB", synonym_limit, filter)
				# if (length(terms) > 0) {
				# 	synonyms <- append(synonyms, getSynonyms(terms[[1]]))
				# }
			
				# print(synonyms)
				# synonyms_cache[[word]] <<- synonyms
			# }
			
			syn_list[[length(syn_list) + 1]] <- synonyms
		}
	}
	
	synonyms_cache[[old_paper]] <<- syn_list
	
	return(syn_list)
}

nwords <- function(paper) {
	word_list <- strsplit(paper, " ")
	return(length(word_list[[1]]))
}

compute_similarity <- function(paperA, paperB) {
	synA <- get_synonyms(paperA)
	synB <- get_synonyms(paperB)
	score <- 0
	
	# count words in common
	for (a in 1:length(synA)) {
		found <- FALSE
		for (b in 1:length(synA[[a]])) {
			for (c in 1:length(synB)) {
				for (d in 1:length(synB[[c]])) {
					if (!is.null(synA[[a]][b]) && !is.null(synB[[c]][d]) && synA[[a]][b] == synB[[c]][d]) {
						found <- TRUE
					}
				}
			}
		}
		if (found == TRUE) {
			score <- score + 1
		}
	}
	
	#normalize
	score <- score / min(nwords(paperA), nwords(paperB))
	
	return(score)
}

