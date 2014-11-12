{-# LANGUAGE OverloadedStrings #-}

import Text.Printf
import Text.Regex.Posix
import Data.Function (on)
import Data.List (sortBy, nubBy)
import Data.Text (intercalate)
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

processData :: [T.Text] -> IO ()
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

symEq :: Eq a => (a, a) -> (a, a) -> Bool
symEq (x,y) (u,v) = (x == u && y == v)

removeDuplTuples :: Eq a => [(a, a)] -> [(a, a)]
removeDuplTuples = nubBy symEq
 
perls_on_russian jokes 
	| elem last_digit [5,6,7,8,9,0] = "перлов"
	| elem last_digit [2,3,4] = "перла"
	| otherwise = "перл"
	where
		last_digit = (jokes `mod` 10)


outputTeachers :: Teacher -> [Char]
outputTeachers (Teacher name jokes) = (printf "%3d %6s %6s %-40s" jokes  (perls :: String) ("--->" :: String) (name :: String) :: String)
	where
		perls = perls_on_russian jokes
							

counter :: PrintfType t => Int -> Int -> t
counter done all = printf ((replicate 16 '\8') ++ "%*d out of %*d" :: String) (length $ show done :: Int) (done :: Int) (length $ show all :: Int) (all :: Int)

main :: IO ()
main = do
	cursor1 <- cursorFor teachersList
	putStrLn "Collecting data ..."
	let
		only = 1019
		titles = map T.unpack $ cursor1 $// findHrefsAndTitles &| extractTitles
		hrefs = map T.unpack $ cursor1 $// findHrefsAndTitles &| extractHrefs
		tuples = take only $ zip [1..] $ removeDuplTuples $ filter ((=~ ("/mephist/prepods.nsf/id/.*" :: String)).snd) $ generateTuples titles hrefs
		tup_len = length tuples
		jokesIO = 
			mapM
				(\(index, tuple) -> do
					c <- cursorFor $ printf baseUrl $ snd tuple
					putStr $ counter index tup_len
					return $ ((fst tuple), real_jokes $ last $ map T.unpack $ c $// findJokesCount &| extractData)
				)
				tuples
	jokes <- jokesIO
	putStrLn "\nAll data collected. Processing ...\n"

	putStrLn . unlines . 
		map 
			(\(index, value) -> (printf "%3s) " $ show index) ++ value ) 
			$ zip [1..]
			$ map outputTeachers 
			$ tuplesToTeachers 
			$ sortOnJokesCount 
			$ filter ((/=0).snd) jokes

