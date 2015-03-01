{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Languages (ScoreType(..), Language(..), score, total) where

import Text.Hamlet (shamlet)
import Text.Regex

data ScoreType = Code {rawCode :: String, desc :: String, enforced :: Bool}
               | Score {humanScore::Integer, desc:: String}
               deriving (Show)

cleanCode c =
  let f = subRegex (mkRegex "<![a-zA-Z-]+!>") c ""
      in subRegex (mkRegex "\\s") f ""

scoreI :: ScoreType -> Integer
scoreI Code {rawCode=c, enforced=enforced} =
  ((if enforced then (subtract 30) else id) . toInteger . length . cleanCode) c
scoreI Score {humanScore=s} = s

score = show . scoreI 

total (Language _ a b c d e f g h i j k l m) =
  sum $ map scoreI [a,b,c,d,e,f,g,h,i,j,k,l,m]

scoreTypeShamlet c@(Code {}) = [shamlet| <p>#{rawCode c} #{desc c}: #{score c}. |]
scoreTypeShamlet st@(Score {}) = [shamlet| <p>#{desc st}: #{score st}. |]

data Language = Language
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
    } deriving (Show)

