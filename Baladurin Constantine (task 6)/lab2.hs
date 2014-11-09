{-# LANGUAGE OverloadedStrings #-}

import Text.Printf
import Text.Regex.Posix
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text as T
import Data.Text (replace)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (
	Cursor, 
	attribute,
	check,
	attributeIs, 
	content, 
	element, 
	fromDocument, 
	child, 
	($//), 
	(&|), 
	(&//),
	(>=>))

newLine = T.pack "\r\n"
teachersListUrl  = "http://cyber.mephi.ru/Faculty.html"
timetableBaseUrl = "http://eis.mephi.ru/TimeTable/timetableshow.aspx?gr=&prep=%s&typ=prep"

notTeachers = [T.pack "Этонашибывшиесотрудники", T.pack "Этоисториякафедры"]

findTeachersNodes :: Cursor -> [Cursor]
findTeachersNodes = element "TD" >=> (attributeIs "bgcolor" "#e1e4e6") &// (element "B" &// element "A") >=> child

findSubjectsNodes :: Cursor -> [Cursor]
findSubjectsNodes = element "span" >=> check isSubject >=> child

extractData :: Cursor -> T.Text
extractData = T.concat . content

isSubject :: Cursor -> Bool
isSubject c = (T.unpack $ head $ attribute "id" c :: String) =~ ("lv_Label4.*" :: String) :: Bool

cursorFor :: String -> IO Cursor
cursorFor u = do
	page <- simpleHttp u
	return $ fromDocument $ parseLBS page

deleteDublicates :: (Eq a) => [a] -> [a]
deleteDublicates = 
	foldl 
		(\acc el -> 
			if elem el acc
			then
				acc
			else
				acc ++ [el]
		) 
		[]

printResults :: IO [T.Text] -> IO ()
printResults subjectsList = do
	putStrLn "123123"

main = do
	teachersCursor <- cursorFor teachersListUrl
	let
		infoNodes = teachersCursor $// findTeachersNodes &| extractData
		
		teachersList = 
			filter
				(\x ->
					let 
						s = T.concat x
					in
						foldl (&&) True $ map (\x -> not $ T.isInfixOf x s) notTeachers
				) $
				map 
					(filter (\el -> el /= "" && el /= newLine)) $ 
					map 
						(T.split (\c -> c == ' ' || c == '\160')) 
						infoNodes
		
		teachersListForTimetable = 
			map
				(\list ->
					let
						(surname:firstName:middleName:xs) = list
						textSpace = T.pack " "
						textPoint = T.pack "."
					in
						if not $ T.isInfixOf "(" firstName
							then 
								T.concat 
									[
										surname, 
										textSpace, 
										T.take 1 firstName, 
										textPoint, 
										textSpace, 
										T.take 1 middleName, 
										textPoint
									]
							else
								T.concat 
									[
										surname, 
										textSpace, 
										T.take 1 middleName, 
										textPoint, 
										textSpace, 
										T.take 1 $ head xs, 
										textPoint
									]
				)
				teachersList
		
		resTeachersList = 
			map
				(\list ->
					let
						stringList = map T.unpack list
					in
						foldl 
							(\acc el -> 
								if acc /= ""
									then acc ++ [' '] ++ el
									else el
							) 
							""
							stringList
				)
				teachersList

		subjectsIOList = 
			mapM
				(\teacher -> do
					c <- cursorFor $ printf timetableBaseUrl $ T.unpack teacher
					return $ map T.unpack $ deleteDublicates $ c $// findSubjectsNodes &| extractData
				)
				teachersListForTimetable

	subjectsList <- subjectsIOList

	mapM_
		(\pair -> do
			let
				(teacherInfo, subjList) = pair
			putStrLn $ teacherInfo ++ ":"
			if subjList /= []
				then mapM_ putStrLn $ map (\x -> "\t" ++ x) subjList
				else putStrLn "\tNoting"
			putStrLn ""
		) $ 
		zip resTeachersList subjectsList