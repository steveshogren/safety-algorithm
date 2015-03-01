{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Languages (ScoreType(..), Language(..), score, total, scoreTypeShamlet) where
import Text.Blaze.Internal (preEscapedText)
import Text.Hamlet (shamlet)
import Text.Regex
import Data.Text (pack)

data ScoreType = Code {rawCode :: String, desc :: String, enforced :: Bool}
               | Score {humanScore::Integer, desc:: String}
               deriving (Show)

cleanCode :: String -> String
cleanCode c =
  let f = subRegex (mkRegex "<![a-zA-Z-]+!>") c ""
      in subRegex (mkRegex "\\s") f ""

scoreI :: ScoreType -> Integer
scoreI Code {rawCode=c, enforced=enforced} =
  ((if enforced then (subtract 30) else id) . toInteger . length . cleanCode) c
scoreI Score {humanScore=s} = s

score :: ScoreType -> String
score = show . scoreI 

total (Language _ _ _ a b c d e f g h i j k l m) =
  sum $ map scoreI [a,b,c,d,e,f,g,h,i,j,k,l,m]

scoreTypeShamlet lang c@(Code {}) =
  let raw = pack . rawCode $ c
      cmt = pack . comment $ lang
      cleaned = (pack . cleanCode . rawCode) c
  in [shamlet|
<p>#{desc c} Score: #{score c} 
\ 
``` #{markupName lang}
\    #{preEscapedText cmt}1234567890123456789012345678901234567890
\    #{preEscapedText cmt}#{preEscapedText cleaned}
\
\    #{preEscapedText raw}
```
\ |]
scoreTypeShamlet _ st@(Score {}) = [shamlet| <p>#{desc st} #{score st} |]

data Language = Language
    { name :: String
    , markupName :: String
    , comment :: String
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

