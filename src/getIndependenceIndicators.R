# main file for computing independence indicators
#
# Anca Dumitrache

source('getlibs.R')
source('parseGoogleScholar.R')
source('parseWikiCFP.R')

library(yaml)


if (file.exists("../img") == FALSE){
  dir.create(file.path("../img"))
}
if (file.exists("../gephi") == FALSE){
  dir.create(file.path("../gephi"))
}

# read configuration file
CONFIG <- yaml.load(readChar("../config.yaml", file.info("../config.yaml")$size))

# get citation indicators
parseGoogleScholar(paste(CONFIG$RESEARCHER_LAST_NAME, CONFIG$RESEARCHER_FIRST_NAME, sep=", "),
                   CONFIG$RESEARCHER_CITATIONS,
                   paste(CONFIG$SUPERVISOR_LAST_NAME, CONFIG$SUPERVISOR_FIRST_NAME, sep=", "),
                   CONFIG$SUPERVISOR_CITATIONS)

# get CFP indicators
parseWikiCFP(paste(CONFIG$RESEARCHER_FIRST_NAME, CONFIG$RESEARCHER_LAST_NAME, sep=" "),
             paste(CONFIG$SUPERVISOR_FIRST_NAME, CONFIG$SUPERVISOR_LAST_NAME, sep=" "))
