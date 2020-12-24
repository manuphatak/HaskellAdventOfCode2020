module Day16.Parser where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Day16.Utils (intParser, toIntMap)
import Text.Parsec

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
        _ <- char '-'
        b <- intParser
        pure [a .. b]

ticketParser :: Parsec String () Ticket
ticketParser = toIntMap <$> (intParser `sepBy1` char ',')

yourTicketParser :: Parsec String () Ticket
yourTicketParser = string "your ticket:" *> newline *> ticketParser

nearbyTicketParser :: Parsec String () [Ticket]
nearbyTicketParser = string "nearby tickets:" *> newline *> (ticketParser `sepEndBy1` newline)
