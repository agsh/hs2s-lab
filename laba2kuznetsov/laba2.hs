{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit (simpleHttp) -- из пакета http-conduit
import qualified Data.ByteString.Lazy.Char8 as L -- если захотим работать с данными, полученными из simpleHttp
import qualified Data.Text as T -- представление юникодной строки в хаскелле
import Data.Text (replace) -- берём из модуля Data.Text только функцию replace (многие ф-ии из него перекрывают станартные из Prelude)
import Text.HTML.DOM (parseLBS) -- из пакета html-conduit
import Text.XML.Cursor (Cursor, attributeIs, content, cut, following, descendant, check, element, fromDocument, attribute, followingSibling, child, ($//), (&|), (&//), (&/), (>=>)) -- из пакета xml-conduit, для работы с DOM-деревом документа
import Network (withSocketsDo) -- workaround для windows
import Text.XML.Light.Cursor (left)

n = T.pack "\r\n" -- перевод строки в плохой вёрстке сайта cyber.mephi.ru

url = "http://en.wikipedia.org/wiki/List_of_programming_languages" -- список языков

wikiURL = "http://en.wikipedia.org" 

-- ставим курсоры на ссылку <a></a> на язык программирования
findNodes :: Cursor -> [Cursor]
findNodes = element "table" >=> attributeIs "class" "multicol" &// element "a"

--извлечь контент из узла
extractData :: Cursor -> T.Text
extractData = T.concat . content

--извлечение ссылок на языки про граммирования
extractDataHref :: Cursor -> T.Text
extractDataHref = T.append wikiURL . T.concat . attribute "href"

findLangNodesT :: Cursor -> [Cursor]
findLangNodesT = element "th" >=> check (\th -> T.pack "Appeared in" `elem` (child th >>= content)) >=> followingSibling >=> child

{-
  Извлекаем содержимое страницы, парсим её и возвращаем курсор на корень DOM-дерева
-}
cursorFor :: String -> IO Cursor -- тут тип важен
cursorFor u = do
     page <- withSocketsDo $ simpleHttp u
     return $ fromDocument $ parseLBS page

putStrLn' a = putStrLn (fst a ++ " " ++ snd a)

goTolink = (\(name,ref) -> do
      c <- cursorFor ref
      let list = c $// findLangNodesT &| extractData
          namelist = T.unpack
          fYear = map (T.unpack)
          k = zip (name:[]) (fYear list) -- конкретно над этим местом я думал очень много, потому что не очень понимаю как работает do нотация
      mapM_ putStrLn' k)                 -- и думал как склеить все что тут есть в список, оказалось это очень просто, но по-моему такой вариант годится только для печати
                                         -- т.е он не создает собственно говоря списка кортежей, т.к насколько я понимаю на каждой итерации свой список из одного кортежа.
                                         -- интересно узнать как сделать списком :)

---------------------------MAIN-------------------------
main = do
  cursor <- cursorFor url
  let getList = cursor $// findNodes &| extractDataHref
      cList = cursor $// findNodes >=> child &| extractData
      filterNodes = map (T.unpack ) . take 40
      filteName = map (T.unpack ) . take 40
      kort = zip (filterNodes cList) (filterNodes getList)
  mapM_ goTolink kort
























