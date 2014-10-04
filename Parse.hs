module Parse where

import Types
import System.IO
import TooSimpleParseLib

-- Lists rule parsers and helper function
ruleSignatures = [(pBorder, extractBorder), (pSize, extractSize)]
pBorder = pSentence [[" borders ", " to the "], [" lies to the ", " of "], [ " is located ", " of "]]
pSize   = pSentence [[" is ", " than "], [" is as ", " as "]]

-- Extracts rule and inverted rule from given string
extractRules :: String -> [Rule]
extractRules a = x ++ (map invRule x)
                 where
                    x = extractRules' a

-- Extracts rules from string
extractRules' :: String -> [Rule]
extractRules' [] = []
extractRules' a = if (not.null) split
                    then ((extractRule x):(extractRules y))
                    else [extractRule a]
                    where
                        (_, (x:y:_)) = head (split)
                        split = runParser (pSentence [[". "], ["."]]) a

-- Extract single rule from string
extractRule :: String -> Rule
extractRule a = extractRule' a ruleSignatures

extractRule' :: String -> [(Parser Char [String], [String] -> Rule)] -> Rule
extractRule' a [] = None
extractRule' a ((x, y):xs) =    if (not.null) parsed
                                    then case rule of
                                            None -> extractRule' a xs
                                            otherwise -> rule
                                    else extractRule' a xs
                                where
                                    parsed = runParser x a
                                    ((_,res):_) = parsed
                                    rule = y res

-- Creates border rule
extractBorder :: [String] -> Rule -- x borders y to the z
extractBorder (x:y:z:_) =   case res of
                                DirFail -> case res2 of
                                            DirFail -> None
                                            otherwise -> Border z res2 x
                                otherwise -> Border x res y
                            where
                                res = toDirection z
                                res2 = toDirection y

-- Creates size rule
extractSize :: [String] -> Rule -- x is y-er than z
extractSize (x:y:z:_) = case res of
                            SizeFail -> None
                            otherwise -> Size x res z
                        where
                            res = toSize y