module InstantRunOff where
import Data.List
import Data.Function
import Data.Functor
import Data.Maybe
import Control.Monad
import Control.Applicative
import Debug.Trace


winners losersPicker ballotSheets
               | ((length $ (nub . join) ballotSheets) == 1) = (head . join) ballotSheets
               | otherwise = winners losersPicker $ map (delete (losersPicker ballotSheets)) ballotSheets

losersOneAtATime ballotSheets =
                let candidates = head ballotSheets
                in
                lowestRankedCandidateDetail candidates (topRankers ballotSheets)

countIn xs x = length (filter (== x) xs)

topRankers ballotSheets = map head ballotSheets

main = do
   putStrLn $ show $ winners losersOneAtATime vm

lowestRankedCandidateDetail candidates topRankers = minimumBy (compare `on` (countIn topRankers)) candidates



