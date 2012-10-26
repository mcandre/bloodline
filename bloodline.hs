#!/usr/bin/env runhaskell

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

import Data.RDF hiding (triple)
import Data.RDF.TriplesGraph

nameSelect :: String -> Query SelectQuery
nameSelect name = do
	rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
	rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
	dbpediaOwl <- prefix "dbpedia-owl" (iriRef "http://dbpedia.org/ontology/")
	dbprop <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")

	programmingLanguage <- var

	let nameLangLit = (name ++ " (programming language)", "en")

	triple programmingLanguage (rdf .:. "type") (dbpediaOwl .:. "ProgrammingLanguage")
	triple programmingLanguage (rdfs .:. "label") nameLangLit

	return SelectQuery { queryVars = [programmingLanguage] }

dbpediaEndpoint :: String
dbpediaEndpoint = "http://dbpedia.org/sparql"

langResource :: String -> IO String
langResource name = do
	results <- selectQuery dbpediaEndpoint (nameSelect name)

	return $ show results

main :: IO ()
main = do
	results <- langResource "D"
	putStrLn $ "Results: " ++ results