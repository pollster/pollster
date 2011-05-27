module InstantRunOff where
import Data.List
import Data.Function
import Data.Functor
import Data.Maybe
import Control.Monad
import Control.Applicative
import Debug.Trace

vm = transpose [
 [1, 1, 2, 2, 3, 3],
 [2, 2, 3, 3, 1, 1],
 [3, 3, 1, 1, 2, 2]
 ]

winners ballotSheets = (majorityCandidate $ topRankers ballotSheets) <|> winners ((map (delete (loser ballotSheets)) ballotSheets))

loser ballotSheets = 
                let candidates = head ballotSheets
                in
                lowestRankedCandidateDetail candidates (topRankers ballotSheets)

countIn xs x = length (filter (== x) xs)

topRankers ballotSheets = map head ballotSheets

main = do
   putStrLn $ show $ winners vm

lowestRankedCandidateDetail candidates topRankers = minimumBy (compare `on` (countIn topRankers)) candidates

majorityCandidate topRanks = join $ listToMaybe <$> (find (\x -> (length x) > (length topRanks) `div` 2) $ group . sort $ topRanks)


