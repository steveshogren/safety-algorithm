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
import Languages (ScoreType(Code,Score), Language(..), score, total)

javascript = 
  Language {
    name = "JavaScript"
    , nullField = Code "if (l !== null) {<!consequent!>} else {<!alternative!>}" "" False
    , nullList = Code "if (l !== null) {<!consequent!>} else {<!alternative!>}" "" False
    , wrongVaribleType = Score 30 ""
    , missingListElem = Code "if (l.length > i) {<!consequent!>} else {<!alternative!>}" "" False
    , wrongCast  = Score 30 "" 
    , wrongTypeToMethod = Score 30 ""
    , missingMethodOrField = Code "if (l !== null) {<!consequent!>} else {<!alternative!>}" "" False
    , missingEnum  = Score 30 ""
    , variableMutation  = Score 0 ""
    , deadLocks  = Score 0 ""
    , memoryDeallocation  = Score (-30) ""
    , recursionStackOverflow  = Score 30 ""
    , consistentCodeExecution = Score (-30) ""
 }   

fsharp = 
  Language {
    name = "F#"
    , nullField = Code "<!consequent!> <*> l" "" True
    , nullList = Score (-30) ""
    , wrongVaribleType = Score (-30) ""
    , missingListElem = Code "if l.Count() > i then <!consequent!> else <!alternative!>" "" False
    , wrongCast  = Code "match o with | :? T as m -> <!consequent!> | _ -> <!alternative!>" "" True
    , wrongTypeToMethod = Score (-30) ""
    , missingMethodOrField  = Score (-30) "" 
    , missingEnum  = Score 0 ""
    , variableMutation  = Score (-30) ""
    , deadLocks  = Score 30 ""
    , memoryDeallocation  = Score (-30) ""
    , recursionStackOverflow  = Score (-30) ""
    , consistentCodeExecution = Score (-30) ""
 }   

csharp = 
  Language {
    name = "C#"
    , nullField = Code "if (l != null) {<!consequent!>} else {<!alternative!>}" "" False
    , nullList = Code "if (l != null) {<!consequent!>} else {<!alternative!>}"  "" False 
    , wrongVaribleType = Score (-30) ""
    , missingListElem = Code "if (l.Count() > i) {<!consequent!>} else {<!alternative!>}" "" False
    , wrongCast  = Code "var m = o as T; if (m != null) {<!consequent!>} else {<!alternative!>}" "" False
    , wrongTypeToMethod = Score (-30) ""
    , missingMethodOrField  = Score (-30) "" 
    , missingEnum  = Score 30 "No way to idiomatically check."
    , variableMutation  = Code "public class T {readonly <!type!> n; public T(<!type!> i) {n = i;}}" "" False
    , deadLocks  = Score 30 ""
    , memoryDeallocation  = Score (-30) "Handled by garbage collector."
    , recursionStackOverflow  = Score 30 ""
    , consistentCodeExecution = Score (-30) ""
 }   

clojure = 
  Language {
    name = "Clojure"
    , nullField = Code "(get l <!lookup-keyword!> <!default-if-missing!>)" "In Clojure, it is idiomatic to put data or functions inside primitive data structures like a hashmap. Retrieval and execution would likely use 'get' which checks for nil by default." False
    , nullList = Score (-30) "In Clojure, the default iteration functions: map, reduce, filter all check and return an empty list if nil, so no need for a check." 
    , wrongVaribleType = Code "(instance? c x)"
                            "In Clojure, the closest thing to a variable is a let bound function or an atom, and neither can be annotated by default. A wrapping call to 'instance?' will give a runtime error."
                            False
    , missingListElem = Code "(get i <!list!> <!default-value!>)" "Clojure's 'get' also gets values out of lists by index." False
    , wrongCast  = Code "(try (<!T!> o) (catch Exception e <!alternative!>))" "Requires a try/catch block around the primitive cast function." False
    , wrongTypeToMethod = Score 0 "In Clojure, parameters can be annotated with a type, which is checked at runtime: "
    , missingMethodOrField  = Score (-30) "Clojure, the language checks for this before runtime." 
    , missingEnum  = Score 30 "No way to idiomatically check."
    , variableMutation  = Score (-30) "In Clojure, anything you would pass is immutable, so no check and enforced by the language before runtime." 
    , deadLocks  = Score (-30) "The STM and agent model built into the language cannot deadlock, and data is immutable or changes are queued."
    , memoryDeallocation  = Score (-30) "Handled by garbage collector."
    , recursionStackOverflow  = Code "(loop [<!params!>] (recur <!args!>))"
                                "Clojure provides a syntax for tail-call opimization, called loop/recur." 
                                False
    , consistentCodeExecution = Score 30 "Clojure macros can prevent parameters from executing at all by rewriting the call, and it is impossible to prevent."
 }   

languages = [csharp, fsharp, clojure, javascript]

tester :: String
tester = renderHtml ($(shamletFile "index.html"))

main :: IO ()
main =
  let file = tester
      in do writeFile "index2.html" file
            

