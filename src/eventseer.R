# test functions for retrieving CFP involvement from Eventseer
#
# Anca Dumitrache

library(SPARQL)

name <- ""

source <- "http://eculture2.cs.vu.nl:8890/sparql"
# source <- "http://redux.eventseer.net:8890/sparql"

# query for conference names
q1 <- paste("select distinct ?ConfName where {
	    	?Conference rdf:type <http://data.semanticweb.org/ns/swc/ontology#AcademicEvent> .
			?Conference rdfs:label ?ConfName .
			?Conference <http://linkedevents.org/ontology/involvedAgent> ?Person .
			?Person rdf:type <http://xmlns.com/foaf/0.1/Person> .
			?Person <http://xmlns.com/foaf/0.1/name> \"",
			name,
			"\" .
			}",
		    sep="", collapse=NULL)
		    
# query for number of conferences
q2 <- paste("select count(distinct ?Conference) where {
			?Conference rdf:type <http://data.semanticweb.org/ns/swc/ontology#AcademicEvent> .
			?Conference <http://linkedevents.org/ontology/involvedAgent> ?Person .
			?Person rdf:type <http://xmlns.com/foaf/0.1/Person> .
			?Person <http://xmlns.com/foaf/0.1/name> \"",
			name,
			"\" .
			}",
		    sep="", collapse=NULL)

d1 <- SPARQL(url=source, query=q1)
d2 <- SPARQL(url=source, query=q2)
