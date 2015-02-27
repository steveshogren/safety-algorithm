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
import Text.Regex

data ScoreType = Code String String | Score Integer String
               deriving (Show, Data, Typeable)

scoreTypeShamlet (Code code desc) = [shamlet| <p>#{code} #{desc}: #{show $ length code}. |]
scoreTypeShamlet (Score score desc) = [shamlet| <p>#{desc}: #{score}. |]

cleanCode c =
  let f = subRegex (mkRegex "<\\w>") c ""
      in subRegex (mkRegex "\\s") f ""

scoreI :: ScoreType -> Integer
scoreI (Code code _) = (toInteger . length . cleanCode) code 
scoreI (Score score _) = score

score = show . scoreI 

total (Codes2 _ a b c d e f g h i j k l m) =
  sum $ map scoreI [a,b,c,d,e,f,g,h,i,j,k,l,m]


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
    , nullList = Score (-30) "In Clojure, the default iteration functions: map, reduce, filter all check and return an empty list if nil, so no need for a check." 
    , wrongVaribleType = Code "(instance? c x)"
                            "In Clojure, the closest thing to a variable is a let bound function or an atom, and neither can be annotated by default. A wrapping call to 'instance?' will give a runtime error." 
    , missingListElem = Code "(get i <list> <default-value>)" "Clojure's 'get' also gets values out of lists by index."
    , wrongCast  = Code "(try (<T> o) (catch Exception e <alternative>))" "Requires a try/catch block around the primitive cast function."
    , wrongTypeToMethod = Score 0 "In Clojure, parameters can be annotated with a type, which is checked at runtime: "
    , missingMethodOrField  = Score (-30) "Clojure, the language checks for this before runtime." 
    , missingEnum  = Score 30 "No way to idiomatically check."
    , variableMutation  = Score (-30) "In Clojure, anything you would pass is immutable, so no check and enforced by the language before runtime." 
    , deadLocks  = Score (-30) "The STM and agent model built into the language cannot deadlock, and data is immutable or changes are queued."
    , memoryDeallocation  = Score (-30) "Handled by garbage collector."
    , recursionStackOverflow  = Code "(loop [<params>] (recur <args>))"
                                "Clojure provides a syntax for tail-call opimization, called loop/recur." 
    , consistentCodeExecution = Score 30 "Clojure macros can prevent parameters from executing at all by rewriting the call, and it is impossible to prevent."
 }   

languages = [clojure]

tester :: String
tester = renderHtml ($(shamletFile "index.html"))

main :: IO ()
main =
  let file = tester
      in do writeFile "index2.html" file
            putStrLn $ show $ total clojure
            

