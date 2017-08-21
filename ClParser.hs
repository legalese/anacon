module ClParser where

import PGF
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import System.Environment

Just langEng   = readLanguage "ClEng"
Just langSym   = readLanguage "ClCLAN"
Just catString = readCId "String"

main = do
  pgf <- readPGF "Cl.pgf"
  let morpho = buildMorpho pgf langEng
  s <- getContents
  args <- getArgs
  case args of
    ["ENG", cat] -> putStrLn (linearizeCLAN pgf (parseEng pgf morpho (fromJust $ readType cat) s))
    ["CLAN",cat] -> putStrLn (linearizeENG  pgf (parseCLAN pgf (fromJust $ readType cat) s))
    _            -> putStrLn "Usage: ClParser (ENG|CLAN) <start category>"

linearizeENG pgf = linearize pgf langEng

parseEng pgf morpho startCat s =
  let ts  = tokenize 0 [] ('\n':s)
      ps0 = initState pgf langEng startCat
  in loop ps0 ts
  where
    loop ps []     =
      case getParseOutput ps startCat Nothing of
        (ParseOk ts,      _) -> head ts
        (ParseFailed n,   _) -> error ("Parsing failed at token "++show n)
        (ParseIncomplete, _) -> error "Incomplete sentence"
        (TypeError _,     _) -> error "Type error"
    loop ps (t:ts) =
      case nextState ps (mkInput (t:ts)) of
        Left  es -> error ("Parse failed at token "++t)
        Right ps -> loop ps ts

    mkInput ts = mkParseInput pgf langEng tok lits ts
      where
        tok (t:ts) = Map.lookup t
        lits = [
           (catString, parseNP)
         ]

        parseNP []         = Nothing
        parseNP ("{":ts)   =
          case break (=="}") ts of
            (ts1,"}":ts2) -> Just (mkStr (unwords ts1), ["{"]++ts1++["}"])
            _             -> Nothing
        parseNP ts         =
          case break (not . null . lookupMorpho morpho) ts of
            (ts1,ts2)     
              | not (null ts1) -> Just (mkStr (unwords ts1), ts1)
              | otherwise      -> Nothing

linearizeCLAN pgf = linearize pgf langSym

parseCLAN pgf startCat s = 
  let ts  = words s
      ps0 = initState pgf langSym startCat
  in loop ps0 ts
  where
    loop ps []     =
      case getParseOutput ps startCat Nothing of
        (ParseOk ts,      _) -> head ts
        (ParseFailed n,   _) -> error ("Parsing failed at token "++show n)
        (ParseIncomplete, _) -> error "Incomplete sentence"
        (TypeError _,     _) -> error "Type error"
    loop ps (t:ts) =
      case nextState ps (mkInput (t:ts)) of
        Left  es -> error ("Parse failed at token "++t)
        Right ps -> loop ps ts

    mkInput ts = mkParseInput pgf langSym tok lits ts
      where
        tok (t:ts) = Map.lookup t
        lits = [
           (catString, parseNP)
         ]

        parseNP ts   =
          case break (=="}") ts of
            (ts1,"}":ts2) -> Just (mkStr (unwords ts1), ts1)
            _             -> Nothing

tokenize n ns [] = replicate (length ns) "}"
tokenize n ns (c:cs)
  | c == '\n'    = let dropSpace n ('\t':cs) = dropSpace (n+8) cs
                       dropSpace n (' ' :cs) = dropSpace (n+1) cs
                       dropSpace n cs        = (n,cs)

                       (n2,cs2) = dropSpace 0 cs
                   in case compare n2 n of
                        GT -> "[" : tokenize n2 (n:ns) cs2
                        EQ ->       tokenize n  ns     cs2
                        LT -> let (ns1,ns2) = break (n2 >) ns
                              in replicate (length ns1) "]" ++ tokenize n2 ns2 cs
  | isSpace    c = tokenize n ns cs
  | isAlphaNum c = let (cs1,cs2) = break (not . isAlphaNum) cs
                   in (c:cs1) : tokenize n ns cs2
  | otherwise    = [c] : tokenize n ns cs
