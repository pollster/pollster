module InstantRunOff where
import Util
import Data.List
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Ord
import Control.Monad
import Control.Applicative
import Debug.Trace

winner losersPicker ballotSheets
               | isSingleton candidates = head candidates
               | otherwise = winner losersPicker $ map (delete loser) ballotSheets
                             where loser = losersPicker ballotSheets
                                   candidates = candidatesFromBallotSheets ballotSheets

losersOneAtATime ballotSheets  =
                let
                  candidates = candidatesFromBallotSheets ballotSheets
                  topRanks = topRanksFromBallotSheets ballotSheets
                in
                lowestRankCandidate candidates topRanks

countIn xs x = length (elemIndices x xs)

topRanksFromBallotSheets ballotSheets = map head (deleteEmptySublists ballotSheets)

candidatesFromBallotSheets ::  (Eq a) => [[a]] -> [a]
candidatesFromBallotSheets = nub . join

lowestRankCandidate candidates topRankers = minimumBy (comparing (countIn topRankers)) candidates

winnerOne ::  Eq a => [[a]] -> a
winnerOne = winner losersOneAtATime

winnerPlurality :: (Eq a) => [[a]] -> a
winnerPlurality ballotSheets = winnerOne $ map (take 1) (deleteEmptySublists ballotSheets)
