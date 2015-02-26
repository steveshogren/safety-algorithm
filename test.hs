-- Just ignore the quasiquote stuff for now, and that shamlet thing.
-- It will be explained later.
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Text.Hamlet (shamlet, shamletFile)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Char (toLower)
import Data.List (sort)
import System.Directory
import Text.JSON.Generic 

data ScoreType = Code String String | Score String Integer 
               deriving (Show, Data, Typeable)

scoreTypeShamlet (Code code desc) = [shamlet| <p>#{code} #{desc}: #{show $ length code}. |]
scoreTypeShamlet (Score desc score) = [shamlet| <p>#{desc}: #{score}. |]

score (Code code _) = show $ length code 
score (Score _ score) = show score

data Codes2 = Codes2
    { name :: String
    , nullField  :: ScoreType
    , nullList    :: ScoreType
    , wrongVaribleType    :: ScoreType
    , missingListElem    :: ScoreType
    , wrongCast    :: ScoreType
    , wrongTypeToMethod    :: ScoreType
    , missingMethodOrField    :: ScoreType
    , missingEnum    :: ScoreType
    , variableMutation    :: ScoreType
    , deadLocks    :: ScoreType
    , memoryDeallocation    :: ScoreType
    , recursionStackOverflow    :: ScoreType
    , consistentCodeExecution    :: ScoreType
    } deriving (Show, Data, Typeable)

contents = do 
  files <- getDirectoryContents "."
  print files

clojure = 
  Codes2 {
    name = "Clojure"
    , nullField = Code "(get l <lookup-keyword> <default-if-missing>)" "In Clojure, it is idiomatic to put data or functions inside primitive data structures like a hashmap. Retrieval and execution would likely use 'get' which checks for nil by default." 
    , nullList = Score "In Clojure, the default iteration functions: map, reduce, filter all check and return an empty list if nil, so no need for a check." (-30)
    , wrongVaribleType = Code "(instance? c x)"
                            "In Clojure, the closest thing to a variable is a let bound function or an atom, and neither can be annotated by default. A wrapping call to 'instance?' will give a runtime error." 
    , missingListElem = Code "(get i <list> <default-value>)" "Clojure's 'get' also gets values out of lists by index."
    , wrongCast  = Code "(try (<T> o) (catch Exception e <alternative>))" "Requires a try/catch block around the primitive cast function."
    , wrongTypeToMethod    = Code "" ""
    , missingMethodOrField  = Code "" ""
    , missingEnum   = Code "" ""
    , variableMutation   = Code "" ""
    , deadLocks  = Code "" ""
    , memoryDeallocation  = Code "" ""
    , recursionStackOverflow  = Code "" ""
    , consistentCodeExecution = Code "" ""
 }   

languages = [clojure]

tester :: String
tester = renderHtml ($(shamletFile "index.html"))

main :: IO ()
main =
  let file = tester
      in do writeFile "index2.html" file
            putStrLn file
            putStrLn $ show clojure

