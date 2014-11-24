{-# LANGUAGE OverloadedStrings #-} 

import Network.HTTP.Conduit (simpleHttp) 
import qualified Data.Text as Text
import Text.HTML.DOM (parseLBS) 
import Text.XML.Cursor  

url = "http://en.wikipedia.org/wiki/Comparison_of_programming_languages"

getDomRoot :: String -> IO Cursor
getDomRoot url = do
	page <- simpleHttp url
     	return $ fromDocument $ parseLBS page

findTableNodes :: Cursor -> [Cursor]
findTableNodes = element "table" >=> attributeIs "class" "wikitable sortable"

findRowNodes :: Cursor -> [Cursor]
findRowNodes = element "tr" 

linkLanguage :: Cursor -> [Cursor]
linkLanguage = element "th" &// element "a" -- for Visual Basic .NET

language :: Cursor -> [Cursor]
language = element "th" 

extractContent :: Cursor -> Text.Text
extractContent = Text.concat . content

filterContent :: Text.Text -> String
filterContent  = Text.unpack 

getLanguageContent :: [Cursor] -> [[String]]
getLanguageContent [] = []
getLanguageContent (x:xs) = (walkThroughNodes 1 x) : getLanguageContent (xs)

walkThroughNodes :: Int -> Cursor -> [String]
walkThroughNodes nodeNum cursor
	| (nodeNum == 1) && ((length (cursor $/ linkLanguage) == 1) || (length (cursor $/ linkLanguage) == 2)) = (head $ cursor $/ linkLanguage >=> child &| filterContent . extractContent) : walkThroughNodes (nodeNum+1) cursor
	| (nodeNum == 1) && (length (cursor $/ language) == 1) = (head $ cursor $/ language >=> child &| filterContent . extractContent) : walkThroughNodes (nodeNum+1) cursor
	| (nodeNum == 2) = walkThroughNodes (nodeNum+1) cursor
	| (nodeNum > 2) && (nodeNum < 10) = do
			let nextCell = head $ drop (nodeNum-2) $ cursor $/ element "td"  
			if (length $ nextCell $.// element "td" >=> hasAttribute "style") == 0
				then "no" : walkThroughNodes (nodeNum+1) cursor
				else "yes" : walkThroughNodes (nodeNum+1) cursor
	| otherwise = []

data Language = Language 	{ name :: String
				, imperative :: String
				, objectOriented :: String
				, functional :: String
				, procedural :: String
				, generic :: String
				, reflective :: String
				, eventDriven :: String}

listOfListsToLanguages :: [[String]] -> [Language]
listOfListsToLanguages [] = []
listOfListsToLanguages (x:xs) = listToLanguage x : listOfListsToLanguages xs

listToLanguage :: [String] -> Language
listToLanguage list = Language {name = elemFromList 1 list
				, imperative = elemFromList 2 list
				, objectOriented = elemFromList 3 list
				, functional = elemFromList 4 list
				, procedural = elemFromList 5 list
				, generic = elemFromList 6 list
				, reflective = elemFromList 7 list
				, eventDriven = elemFromList 8 list}

elemFromList :: (Eq a1, Num a, Ord a) => a -> [a1] -> a1
elemFromList pos list =
	let elemFromList pos curPos list 
				| pos <= 0 = error "Position must be positive"
				| list == [] = error "Posotion must be not more than length of the list"
				| pos == curPos = head list
				| otherwise = elemFromList pos (curPos+1) $ tail list 
	in elemFromList pos 1 list 

formImperetiveNonFunctionalList :: [Language] -> [String]
formImperetiveNonFunctionalList [] = []
formImperetiveNonFunctionalList (x:xs)
	| (imperative x == "yes") && (functional x == "no") = name x : formImperetiveNonFunctionalList xs
	| otherwise = formImperetiveNonFunctionalList xs

outputList :: [String] -> IO ()
outputList [] = return()
outputList (x:xs) = 
	let 
		outputList _ [] = return()
		outputList 0 _ = do 
			putStrLn "List of imperative but non-functional programming languages:"
			outputList 1 xs
		outputList num (x:xs) = do
			putStr $ show num;
			putStr ". ";
			putStrLn x;
			outputList (num+1) xs
	in outputList 0 (x:xs)

-------------------------------Main block----------------------------------------

main = do
 	root <- getDomRoot url 
	let 	
				table = cut $ head $ root $// findTableNodes -- main table
				rowNodes = table $// findRowNodes  -- row nodes [Cursor]
				tableContent = filter (/= []) $ getLanguageContent rowNodes -- [[String]]
				languageList = listOfListsToLanguages tableContent
				imperativeNonFunctional = formImperetiveNonFunctionalList languageList
	outputList imperativeNonFunctional
	putStrLn "End of the list"
