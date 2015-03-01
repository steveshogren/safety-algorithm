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
import Languages (ScoreType(Code,Score), Language(..), score, total, scoreTypeShamlet)

javascript = 
  Language {
    name = "JavaScript"
    , markupName = "javascript"
    , comment = "//"
    , nullField = Code "if (l !== null) {<!consequent!>} else {<!alternative!>}" "Javascript the common pattern is to check if something is there with an OR statement." False
    , nullList = Code "if (l !== null) {<!consequent!>} else {<!alternative!>}" "Same check as for a null field." False
    , wrongVaribleType = Score 30 "No real idiomatic way to check."
    , missingListElem = Code "if (l.length > i) {<!consequent!>} else {<!alternative!>}" "" False
    , wrongCast  = Score 30 "No real idiomatic way to check." 
    , wrongTypeToMethod = Score 30 "No real idiomatic way to check."
    , missingMethodOrField = Code "t.f || <alternative>" "It is common to use the OR statement to get a field OR something else if it isn't there or empty." False
    , missingEnum  = Score 30 "No way to idiomatically check."
    , variableMutation  = Code "" "In JavaScript, we would have to make the field inside an object, and use an accessor to expose it." False
    , deadLocks  = Score 0 "Javascript is single threaded, and uses a queue for asynchronous execution responses like from calls to Ajax methods. As such, deadlocks are not possible by design. Javascript therefore is restricted in its abilities, but this is about categorizing safety only."
    , memoryDeallocation  = Score (-30) "Handled By Garbage Collector."
    , recursionStackOverflow  = Score 30 "No way to prevent these, and therefore the alternative is to write algorithms in a loop construct. It is not idiomatic to use recursion because of this. While any recursive algorithm can be expressed in a loop, it can require more size and possibly a less intuitive algorithm."
    , consistentCodeExecution = Score (-30) "Compiler Enforced."
 }   

fsharp = 
  Language {
    name = "F#"
    , markupName = "fsharp"
    , comment = "//"
    , nullField = Code "<!consequent!> <*> l" "In F#, it is idiomatic to use Option instead of null (most classes cannot be made null without special effort). The FSharpx library function 'sequential application' written: (<*>) automatically tests for Some or None, and applies the consequent only if the value is Some." True
    , nullList = Score (-30) "In F#, the idiomatic list cannot be made null by the compiler, so there is no check."
    , wrongVaribleType = Score (-30) "Compiler Enforced."
    , missingListElem = Code "if l.Count() > i then <!consequent!> else <!alternative!>" "" False
    , wrongCast  = Code "match o with | :? T as m -> <!consequent!> | _ -> <!alternative!>" "" True
    , wrongTypeToMethod = Score (-30) "Compiler Enforced."
    , missingMethodOrField  = Score (-30) "Compiler Enforced." 
    , missingEnum  = Score 0 "The compiler offers this as a warning with no extra code (but it is not enforced)."
    , variableMutation  = Score 0 "In F# we idiomatically would use whatever fit the need most: an existing class, a let bound primitive, a tuple, etc rather than make a whole class just for the immutability. F# class fields and values are immutable by default, so nothing extra." 
    , deadLocks  = Score 30 "As far as I know, there is provide any way to prevent deadlocks at the compiler level, and it may not be possible, but it gets scored."
    , memoryDeallocation  = Score (-30) "Handled By Garbage Collector."
    , recursionStackOverflow  = Score (-30) "F# recursive functions calls are converted into loop constructs by the compiler automatically."
    , consistentCodeExecution = Score (-30) "Compiler Enforced."
 }   

csharp = 
  Language {
    name = "C#"
    , markupName = "csharp"
    , comment = "//"
    , nullField = Code "if (l != null) {<!consequent!>} else {<!alternative!>}"
                  "It is possible to use the ternary operator as well, but a quick StackOverflow search shows a lot of comments cautioning against using them 'too much', so we will count the traditional 'if-else' for the most idiomatic way of checking if the field is null before using it." False
    , nullList = Code "if (l != null) {<!consequent!>} else {<!alternative!>}" "Same check as for a null field." False 
    , wrongVaribleType = Score (-30) "Compiler Enforced."
    , missingListElem = Code "if (l.Count() > i) {<!consequent!>} else {<!alternative!>}" "" False
    , wrongCast  = Code "var m = o as T; if (m != null) {<!consequent!>} else {<!alternative!>}" "" False
    , wrongTypeToMethod = Score (-30) "Compiler Enforced."
    , missingMethodOrField  = Score (-30) "Compiler Enforced." 
    , missingEnum  = Score 30 "For example, using a switch-case in C# to dispatch on an enum value. If you add a new value, the compiler does nothing, so no safety. It isn't idiomatically possible to prevent this error."
    , variableMutation  = Code "public class T {readonly <!type!> n; public T(<!type!> i) {n = i;}}" "For example, I pass data to a function, will the data come back the same as I passed it, or will it have mutated in some way? To prevent this, in C#, we would idiomatically make a new class and make the field readonly." False
    , deadLocks  = Score 30 "As far as I know, there is provide any way to prevent deadlocks at the compiler level, and it may not be possible, but it gets scored."
    , memoryDeallocation  = Score (-30) "Handled by garbage collector."
    , recursionStackOverflow  = Score 30 "No way to prevent these, and therefore the alternative is to write algorithms in a loop construct. It is not idiomatic to use recursion because of this. While any recursive algorithm can be expressed in a loop, it can require more size and possibly a less intuitive algorithm."
    , consistentCodeExecution = Score (-30) "Compiler Enforced."
 }   

clojure = 
  Language {
    name = "Clojure"
    , markupName = "clojure"
    , comment = ";;"
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
            

