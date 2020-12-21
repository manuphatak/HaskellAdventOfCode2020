module Day16.Solution where

import Advent.Utils
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Text.Parsec

part1 :: String -> String
part1 = show . sum . ticketScanningErrors . fromRightOrShowError . parseDocument

part2 :: String -> String
part2 = head . lines

type Ticket = IntMap.IntMap Int

type DocumentFields = Map.Map String IntSet.IntSet

data Document = Document
  { dFields :: DocumentFields,
    dYourTicket :: Ticket,
    dNearbyTickets :: [Ticket]
  }
  deriving (Show, Eq)

parseDocument :: String -> Either ParseError Document
parseDocument = parse documentParser ""

documentParser :: Parsec String () Document
documentParser =
  Document
    <$> (fieldsParser <* many1 newline)
    <*> (yourTicketParser <* many1 newline)
    <*> (nearbyTicketParser <* eof)

fieldsParser :: Parsec String () DocumentFields
fieldsParser = Map.fromList <$> fieldParser `sepEndBy1` newline
  where
    fieldParser :: Parsec String () (String, IntSet.IntSet)
    fieldParser = (,) <$> (many1 (letter <|> char ' ') <* string ": ") <*> fieldValueParser

    fieldValueParser :: Parsec String () IntSet.IntSet
    fieldValueParser = IntSet.unions <$> rangeParser `sepBy` string " or "

    rangeParser :: Parsec String () IntSet.IntSet
    rangeParser =
      IntSet.fromList <$> do
        a <- intParser
        char '-'
        b <- intParser
        pure [a .. b]

ticketParser :: Parsec String () Ticket
ticketParser = toIntMap <$> (intParser `sepBy1` char ',')

yourTicketParser :: Parsec String () Ticket
yourTicketParser = string "your ticket:" *> newline *> ticketParser

nearbyTicketParser :: Parsec String () [Ticket]
nearbyTicketParser = string "nearby tickets:" *> newline *> (ticketParser `sepEndBy1` newline)

intParser :: Parsec String () Int
intParser = readInt <$> many1 digit

toIntMap :: [Int] -> IntMap.IntMap Int
toIntMap = IntMap.fromList . zip [0 ..]

ticketScanningErrors :: Document -> [Int]
ticketScanningErrors
  Document
    { dFields = fields,
      dNearbyTickets = nearbyTickets
    } = filter (`IntSet.notMember` allFields) . concatMap (foldr (:) []) $ nearbyTickets
    where
      allFields = foldr1 IntSet.union fields
