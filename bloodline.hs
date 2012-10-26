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

	-- triple programmingLanguage (rdf .:. "type") (dbpediaOwl .:. "ProgrammingLanguage")
	triple programmingLanguage (rdfs .:. "label") nameLangLit

	return SelectQuery { queryVars = [programmingLanguage] }

influencedBySelect :: String -> Query SelectQuery
influencedBySelect name = do
	rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
	rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
	dbpediaOwl <- prefix "dbpedia-owl" (iriRef "http://dbpedia.org/ontology/")
	dbprop <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")

	programmingLanguage <- var
	influencedBy <- var

	let nameLangLit = (name ++ " (programming language)", "en")

	triple programmingLanguage (rdfs .:. "label") nameLangLit
	triple programmingLanguage (dbpediaOwl .:. "influencedBy") influencedBy

	return SelectQuery { queryVars = [influencedBy] }

influencedSelect :: String -> Query SelectQuery
influencedSelect name = do
	rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
	rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
	dbpediaOwl <- prefix "dbpedia-owl" (iriRef "http://dbpedia.org/ontology/")
	dbprop <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")

	programmingLanguage <- var
	influenced <- var

	let nameLangLit = (name ++ " (programming language)", "en")

	triple programmingLanguage (rdfs .:. "label") nameLangLit
	triple programmingLanguage (dbpediaOwl .:. "influenced") influenced

	return SelectQuery { queryVars = [influenced] }

dbpediaEndpoint :: String
dbpediaEndpoint = "http://dbpedia.org/sparql"

-- like Data.Maybe (catMaybes)
catBounds :: [BindingValue] -> [Node]
catBounds ls = [x | Bound x <- ls]

unbind :: Maybe [[BindingValue]] -> [[Node]]
unbind results = case results of
	Nothing -> []
	Just results' -> map catBounds results'

langResource :: String -> IO String
langResource name = do
	results <- selectQuery dbpediaEndpoint (nameSelect name)
	return $ show $ unbind results

langParents :: String -> IO String
langParents name = do
	results <- selectQuery dbpediaEndpoint (influencedBySelect name)
	return $ show $ unbind results

langChildren :: String -> IO String
langChildren name = do
	results <- selectQuery dbpediaEndpoint (influencedSelect name)
	return $ show $ unbind results

main :: IO ()
main = do
	let language = "D"

	resource <- langResource language

	putStrLn $ "Resource: " ++ resource ++ "\n"

	parents <- langParents language

	putStrLn $ "Parents: " ++ parents ++ "\n"

	children <- langChildren language

	putStrLn $ "Children: " ++ children