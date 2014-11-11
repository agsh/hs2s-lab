{-# LANGUAGE OverloadedStrings #-}

import Text.Printf
import Text.Regex.Posix
import Data.Function (on)
import Data.List (sortBy, nubBy)
import Network.HTTP.Conduit (simpleHttp)
import Codec.Text.IConv as IConv
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element, child, attribute, fromDocument,
                        ($//), (&|), (&//), (>=>))


data Teacher = Teacher { name :: String
                     , jokesCount :: Int   
                     } deriving (Show)

teachersList = "http://www.mephist.ru/mephist/prepods.nsf/teachers"
baseUrl = "http://www.mephist.ru%s"

noJokes = "Пришлите"

findHrefsAndTitles :: Cursor -> [Cursor]
findHrefsAndTitles = element "TD" >=> (attributeIs "class" "menu1") &// element "a"

findJokesCount :: Cursor -> [Cursor]
findJokesCount = element "table" >=> (attributeIs "class" "MphDataTable") &// element "a" >=> child

isJokeRow :: Cursor -> Bool
isJokeRow c = (T.unpack $ head $ attribute "href" c :: String) =~ (".*bash?Open.*" :: String) :: Bool

extractTitles = T.concat . attribute "title"
extractHrefs = T.concat . attribute "href"
extractData = T.concat . content

processData = putStrLn . T.unpack . T.concat

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- simpleHttp u
     let rus = IConv.convert "CP1251" "UTF-8" page
     return $ fromDocument $ parseLBS rus

generateTuples :: [String] -> [String] -> [(String, String)]
generateTuples _ [] = []
generateTuples [] _ = []
generateTuples (x:xs) (y:ys) = (x, y): generateTuples xs ys

{- parseHtmlWithJokesCount :: [Char] -> IO String
parseHtmlWithJokesCount url_part = do
	c <- cursorFor $ (baseUrl ++ url_part)
	return $ last $ map T.unpack $ c $// findJokesCount &| extractData -}

getJokesCount :: [(String, String)] -> [(String, Int)]
getJokesCount [] = []
getJokesCount (teacher:teachers) = (fst teacher, real_jokes $ "1"):getJokesCount teachers

real_jokes :: [Char] -> Int
real_jokes j | j == noJokes = 0
real_jokes j 				= read j

sortOnJokesCount :: Ord b => [(a, b)] -> [(a, b)]
sortOnJokesCount arr = reverse $ sortBy (compare `on` snd) arr

tuplesToTeachers :: [(String, Int)] -> [Teacher]
tuplesToTeachers [] = []
tuplesToTeachers ((name, jokes):xs) = Teacher name jokes : tuplesToTeachers xs

symEq :: (String, Int) -> (String, Int) -> Bool
symEq (x,y) (u,v) = (x == u && y == v)

removeDuplTuples :: [(String, Int)] -> [(String, Int)]
removeDuplTuples = nubBy symEq

outputTeachers :: Teacher -> [Char]
outputTeachers (Teacher name jokes) = (printf "%3d %6s %10s %40s" jokes  (perls :: String) ("--->" :: String) (name :: String) :: String)
	where
		perls = if elem last_digit [5,6,7,8,9,0]
				then
					"перлов"
				else
					if elem last_digit [2,3,4]
					then
						"перла"
					else
						"перл"
						where
							last_digit = (jokes `mod` 10)

main = do
	cursor1 <- cursorFor teachersList
	let
		only = 1100
		titles = map T.unpack $ cursor1 $// findHrefsAndTitles &| extractTitles
		hrefs = map T.unpack $ cursor1 $// findHrefsAndTitles &| extractHrefs
		tuples = take only $ filter ((=~ ("/mephist/prepods.nsf/id/.*" :: String)).snd) $ generateTuples titles hrefs
		jokesIO = 
			mapM
				(\tuple -> do
					c <- cursorFor $ printf baseUrl $ snd tuple
					putStr "."
					return $ ((fst tuple), real_jokes $ last $ map T.unpack $ c $// findJokesCount &| extractData)
				)
				tuples
	jokes <- jokesIO
	putStrLn " Collected all data. Processing ..."

	putStrLn . unlines . 
		map 
			(\(index, value) -> (printf "%3s) " $ show index) ++ value ) 
			$ zip [1..] 
			$ map outputTeachers 
			$ tuplesToTeachers 
			$ removeDuplTuples
			$ sortOnJokesCount 
			$ filter ((/=0).snd) jokes
