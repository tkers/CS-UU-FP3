{-
This module is to be used for demonstration purposes only. For real applications you may consider the
uu-parsinglib package available from HackageDB
-}

module TooSimpleParseLib (module Control.Applicative, module Data.Functor, module TooSimpleParseLib) where
import Data.Functor
import Control.Applicative -- includes Alternative
import Data.Char
import Types

infixl 2 `opt`

-- | The type Parser

newtype Parser s t = P {runParser :: [s] -> [([s], t)]}

-- Parsers are instances of common classes
	
instance Functor (Parser s) where
	fmap f p = P $ \s -> [(ss', f a) | (ss', a) <- runParser p s]

instance Applicative (Parser s) where
	p <*> q = P $ \ss -> [ (ss'', b2a b) | (ss', b2a) <- runParser p ss,
	 (ss'', b) <- runParser q ss']
	pure a = P $ \ss -> [(ss, a)]

instance Alternative (Parser s) where
	p <|> q = P $ \xs -> runParser p xs ++ runParser q xs
	empty = P $ const []

-- Basic parsers

pSym :: Eq s => s -> Parser s s
pSym a = pSatisfy (== a)
	
pSatisfy :: (s -> Bool) -> Parser s s
pSatisfy p = P $ \xs -> case xs of
	(x:xs') | p x -> [(xs', x)]
	_ -> []

pToken :: Eq s => [s] -> Parser s [s]
pToken k = P $ \xs -> let n = length k 
	in if k == take n xs then [(drop n xs,k)] else []

pNotToken :: Eq s => [s] -> Parser s [s]
pNotToken k = P $ takeToToken k []

takeToToken :: Eq a => [a] -> [a] -> [a] -> [([a], [a])]
takeToToken [] _ i = [([], i)]
takeToToken k p i@(x:xs) = if k /= take (length k) i
							then takeToToken k (p ++ [x]) xs
							else [(i, p)]
takeToToken k p [] = [([], p)]

-- Applications of elementary parsers

pDigit :: Parser Char Char
pDigit = pSatisfy (\x -> ord '0' <= ord x && ord x <= ord '9')

pDigAsInt :: Parser Char Int
pDigAsInt = (\c -> ord c - ord '0') <$> pDigit

-- Some common

opt :: Parser s a -> a -> Parser s a
opt p d = p <|> pure d

pPack :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pPack p r q = p *> r <* q 

pListSep :: Parser s a -> Parser s b -> Parser s [a]
pListSep p s = (:) <$> p <*> many ( s *> p)

-- Auxiliary functions

determ :: Parser s b -> Parser s b
determ p = P $ \xs -> let r = runParser p xs
	in if null r then [] else [head r]


-- Applications of EBNF combinators

pNatural :: Parser Char Int
pNatural = foldl (\a b -> a*10 + b) 0 <$> some pDigAsInt

pInteger :: Parser Char Int
pInteger = ((negate <$ (pSym '-')) `opt` id ) <*> pNatural 

pIdentifier :: Parser Char String
pIdentifier = (:) <$> pSatisfy isAlpha <*> many (pSatisfy isAlphaNum)

pParens :: Parser Char b -> Parser Char b
pParens p = pPack (pSym '(') p (pSym ')')

pCommaList :: Parser Char a -> Parser Char [a]
pCommaList p = pListSep p (pSym ',')

pSequence :: [Parser s a] -> Parser s [a]
pSequence [] = pure []
pSequence (p:ps) = (:) <$> p <*> pSequence ps

pChoice :: [Parser s a] -> Parser s a
pChoice = foldr (<|>) empty

-- Compatibality, we like parsers to start with a lower case p

pSucceed :: a -> Parser s a
pSucceed = pure

pFail :: Parser s a
pFail = empty 

--pDirection = pToken "North" <|> pToken "South" <|> pToken "East" <|> pToken "West"

pSentence :: [[String]] -> Parser Char [String]
pSentence (x:[]) = pSentence' x
pSentence (x:xs) = pSentence' x <|> pSentence xs

pSentence' a = pSequence ((pSentence'' a) ++ [pNotToken ""])
pSentence'' [] = []
pSentence'' (x:xs) = ((pNotToken x <* pToken x) : pSentence'' xs)