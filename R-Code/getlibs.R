# Install project libraries
# Needs to be run with admin privileges
#
# Anca Dumitrache
# 18/10/2012

# RMendeley libs
# install.packages("devtools")
# library(devtools)
# install_github("ROAUth", "ropensci")
# install.packages("RMendeley")

is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 

package_list <- c(
                  # graph analysis lib
                  "igraph", "SnowballC", "yaml",
                  # NLP libs
                  "tm", "wordnet", "openNLP", 
                  # crawling packages for WikiCFP
                  "RCurl", "XML", "rjson", "RecordLinkage","stringr",
                  #Mendeley
                  "RMendeley")

# graph analysis lib
for (i in 1:length(package_list)) {
  #print(package_list[i])
  if (is.installed(package_list[i]) == FALSE) {
    install.packages(package_list[i])
  }
}

# library(wordnet)
## set WordNet dictionary
# setDict("../lib/WordNet")

