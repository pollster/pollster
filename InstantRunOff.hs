module InstantRunOff where
import Data.List
import Data.Function
import Data.Functor
import Data.Maybe
import Control.Monad
import Control.Applicative
import Debug.Trace

winner losersPicker ballotSheets
               | (length (join ballotSheets) == 0) = error "WTF"
               | ((length (candidatesFromBallotSheets ballotSheets)) == 1) = (head . join) ballotSheets
               | otherwise = winner losersPicker $ map (delete (losersPicker ballotSheets)) ballotSheets

losersOneAtATime ballotSheets  =
                let
                  candidates = candidatesFromBallotSheets ballotSheets
                  topRankers = topRankersFromBallotSheets ballotSheets
                in
                lowestRankedCandidateDetail candidates topRankers

countIn xs x = length (filter (== x) xs)

topRankersFromBallotSheets ballotSheets = map head $ filter (\xs -> length xs /= 0) ballotSheets

candidatesFromBallotSheets ballotSheets = (nub . join) ballotSheets

lowestRankedCandidateDetail candidates topRankers = minimumBy (compare `on` (countIn topRankers)) candidates



