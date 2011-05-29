module InstantRunOff where
import Data.List
import Data.Function
import Data.Functor
import Data.Maybe
import Control.Monad
import Control.Applicative
import Debug.Trace


winner losersPicker ballotSheets
               | (null . join) ballotSheets = error "WTF"
               | (length . candidatesFromBallotSheets) ballotSheets == 1 = (head . join) ballotSheets
               | otherwise = winner losersPicker $ map (delete loser) ballotSheets
                             where loser = losersPicker ballotSheets

losersOneAtATime ballotSheets  =
                let
                  candidates = candidatesFromBallotSheets ballotSheets
                  topRankers = topRankersFromBallotSheets ballotSheets
                in
                lowestRankedCandidateDetail candidates topRankers

countIn xs x = length (elemIndices x xs)

topRankersFromBallotSheets ballotSheets = map head $ filter (not . null) ballotSheets

candidatesFromBallotSheets ballotSheets = (nub . join) ballotSheets

lowestRankedCandidateDetail candidates topRankers = minimumBy (compare `on` (countIn topRankers)) candidates

winnerOne :: (Eq a) => [[a]] -> a
winnerOne = winner losersOneAtATime

winnerPlurality :: (Eq a) => [[a]] -> a
winnerPlurality ballotSheets = winnerOne $ map ((:[]) . head) (filter (not. null) ballotSheets)
