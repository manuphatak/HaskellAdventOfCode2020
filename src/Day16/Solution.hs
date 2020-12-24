module Day16.Solution
  ( Document (..),
    ValidDocument (..),
    asValidDocument,
    extractTicket,
    parseDocument,
    part1,
    part2,
    resolvePossibilities,
    ticketScanningErrors,
  )
where

import Advent.Utils (fromRightOrShowError)
import Data.Bifunctor (Bifunctor (second))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map
import Day16.Parser (Document (..), Ticket, parseDocument)

part1 :: String -> String
part1 = show . sum . ticketScanningErrors . fromRightOrShowError . parseDocument

part2 :: String -> String
part2 = show . product . Map.filterWithKey (const . isPrefixOf "departure") . extractTicket . asValidDocument . fromRightOrShowError . parseDocument

newtype ValidDocument = ValidDocument Document deriving (Show, Eq)

ticketScanningErrors :: Document -> [Int]
ticketScanningErrors
  Document
    { dFields = fields,
      dNearbyTickets = nearbyTickets
    } = filter (`IntSet.notMember` allFields) . concatMap (foldr (:) []) $ nearbyTickets
    where
      allFields = foldr1 IntSet.union fields

asValidDocument :: Document -> ValidDocument
asValidDocument
  document@Document
    { dFields = fields,
      dNearbyTickets = nearbyTickets
    } = ValidDocument document {dNearbyTickets = filter validTicket nearbyTickets}
    where
      allFields = foldr1 IntSet.union fields

      validTicket :: Ticket -> Bool
      validTicket = all (`IntSet.member` allFields)

extractTicket :: ValidDocument -> Map.Map String Int
extractTicket
  ( ValidDocument
      Document
        { dFields = fields,
          dYourTicket = yourTicket,
          dNearbyTickets = nearbyTickets
        }
    ) = Map.map (yourTicket IntMap.!) fieldColumnMap
    where
      fieldColumnMap :: Map.Map String Int
      fieldColumnMap = resolvePossibilities . Map.map toPossibleColumns $ fields

      toPossibleColumns :: IntSet.IntSet -> [Int]
      toPossibleColumns fieldValues = filter (all (`IntSet.member` fieldValues) . valuesForColumn) allColumns

      valuesForColumn :: Int -> [Int]
      valuesForColumn i = pluck i nearbyTickets

      allColumns :: [Int]
      allColumns = [0 .. (length yourTicket - 1)]

      pluck :: Int -> [Ticket] -> [Int]
      pluck i = map (IntMap.! i)

resolvePossibilities :: Map.Map String [Int] -> Map.Map String Int
resolvePossibilities input = go ((sortByResolvability . Map.toList) input) Map.empty
  where
    sortByResolvability :: [(String, [Int])] -> [(String, [Int])]
    sortByResolvability = sortOn (length . snd)

    go :: [(String, [Int])] -> Map.Map String Int -> Map.Map String Int
    go [] output = output
    go (x : unresolved) output = go nextUnresolved nextOutput
      where
        (key, value) = second head x

        nextUnresolved :: [(String, [Int])]
        nextUnresolved = sortByResolvability $ map (second (filter (/= value))) unresolved

        nextOutput :: Map.Map String Int
        nextOutput = Map.insert key value output
