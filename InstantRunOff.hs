module InstantRunOff where
import Data.List
import Data.Function

vm = transpose [
 [1, 3, 1, 1, 2],
 [2, 2, 2, 3, 1],
 [3, 1, 3, 2, 1]
 ]

winners ballotSheets =
                let loserCandidate = loser ballotSheets
                in
                map (delete loserCandidate) ballotSheets

loser ballotSheets = 
                let candidates = head ballotSheets
                    topRankers = map head ballotSheets
                in
                minimumBy (compare `on` (countIn topRankers)) candidates

countIn :: (Eq a) => [a] -> a -> Int
countIn xs x = length (filter (== x) xs)

main = do
   putStrLn $ show $ winners vm
