--
-- Primitives and combinators to build parsers
--
-- Inspired by Thesz (thesz.livejournal.com)
--

module Parser(
  ParseResult,
  Parser,
  runParser,
  takeFirstResult,
  parseFail,
  parseEmpty,
  parseItem,
  parseConst,
  parseFilter,
  parseMap,
  parseUnion,
  parseBind,
  parseMerge,
  parseJoin
) where

import Control.Applicative
import Data.Tuple.HT (mapFst, mapSnd)

type ParseResult a b = (b, [a])
type ParserImpl a b = [a] -> [ParseResult a b]

newtype Parser a b = Parser { runParser :: ParserImpl a b }

instance Functor (Parser a) where
  fmap = parseMap

instance Applicative (Parser a) where
  pure  = parseEmpty
  (<*>) = parseJoin

instance Alternative (Parser a) where
  empty = parseFail
  (<|>) = parseUnion

instance Monad (Parser a) where
  return = parseEmpty
  (>>=)  = parseBind

takeFirstResult :: Show a => [ParseResult a b] -> b
takeFirstResult [] = error "No parse at all"
takeFirstResult ((r,[]):_) = r
takeFirstResult ((r,s):_) = error $ "Stopped at " ++ (show $ take 20 s)

parseFail :: Parser a b
parseFail = Parser parseFailImpl

parseFailImpl :: ParserImpl a b
parseFailImpl _ = []

parseEmpty :: b -> Parser a b
parseEmpty = Parser . parseEmptyImpl

parseEmptyImpl :: b -> ParserImpl a b
parseEmptyImpl x s = [(x, s)]

parseItem :: Parser a a
parseItem = Parser parseItemImpl

parseItemImpl :: ParserImpl a a
parseItemImpl [] = []
parseItemImpl (x:xs) = [(x, xs)]

parseConst :: (Eq a) =>  a -> Parser a a
parseConst x = parseFilter (==x) parseItem

parseFilter :: (b -> Bool) -> Parser a b -> Parser a b
parseFilter cond (Parser parser) = Parser $ parseFilterImpl cond parser

parseFilterImpl :: (b -> Bool) -> ParserImpl a b -> ParserImpl a b
parseFilterImpl cond parser = filter (cond . fst) . parser

parseMap :: (b -> c) -> Parser a b -> Parser a c
parseMap f (Parser parser) = Parser $ parseMapImpl f parser

parseMapImpl :: (b -> c) -> ParserImpl a b -> ParserImpl a c
parseMapImpl f parser = map (mapFst f) . parser

parseUnion :: Parser a b -> Parser a b -> Parser a b
parseUnion (Parser parser1) (Parser parser2) = Parser $ parseUnionImpl parser1 parser2

parseUnionImpl :: ParserImpl a b -> ParserImpl a b -> ParserImpl a b
parseUnionImpl parser1 parser2 s = (parser1 s) ++ (parser2 s)

parseBind :: Parser a b -> (b -> Parser a c) -> Parser a c
parseBind (Parser parser) continuator = Parser $ parseBindImpl parser (runParser . continuator)

parseBindImpl :: ParserImpl a b -> (b -> ParserImpl a c) -> ParserImpl a c
parseBindImpl parser continuator = concat . map (uncurry continuator) . parser

parseMerge :: (b -> c -> d) -> Parser a b -> Parser a c -> Parser a d
parseMerge merger (Parser parser1) (Parser parser2) = Parser $ parseMergeImpl merger parser1 parser2

parseMergeImpl :: (b -> c -> d) -> ParserImpl a b -> ParserImpl a c -> ParserImpl a d
parseMergeImpl merger parser1 parser2 =
  concat .
  map (uncurry (map . mapFst)) .
  map (mapFst merger) .
  map (mapSnd parser2) .
  parser1

parseJoin :: Parser a (b -> c) -> Parser a b -> Parser a c
parseJoin (Parser parser1) (Parser parser2) = Parser $ parseJoinImpl parser1 parser2

parseJoinImpl :: ParserImpl a (b -> c) -> ParserImpl a b -> ParserImpl a c
parseJoinImpl parser1 parser2 =
  concat .
  map (uncurry (map . mapFst)) .
  map (mapSnd parser2) .
  parser1

