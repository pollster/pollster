import InstantRunOff
import Test.QuickCheck
import Control.Monad(join)
import Control.Monad.Trans
import Control.Monad
import Data.List

prop_Simple w = forAll ballotSheetsSimple $ (\xss -> (not . null . join) xss ==> w xss `elem` join xss)
prop_MajorityCriteria w = forAll ballotSheetsMajorityCriteria $ (\xss -> (not . null . join) xss  ==> w (map ((:[]) . head) (filter (not . null) xss)) == w xss)

ballotSheetsSimple :: Gen [[Int]]
ballotSheetsSimple = do
      res <- arbitrary
      return $ (fmap $ (nub . (fmap (`mod` 5)))) res

ballotSheetsMajorityCriteria :: Gen [[Int]]
ballotSheetsMajorityCriteria = do
      majorityCandidate <- arbitrary
      arbitraryList <- fmap (fmap (fmap (`mod` 5)) ) arbitrary
      majority <- elements [smallestPossibleMajority(length arbitraryList)..length arbitraryList]
      return $ fmap nub (zipWith ($) ((replicate majority ((majorityCandidate `mod` 5):)) ++ (repeat id)) arbitraryList)

smallestPossibleMajority total
                           | total == 0 = 0
                           | otherwise = (total `div` 2) + 1

main = do
      sequence $ map quickCheck [
               prop_Simple winnerOne,
               prop_Simple winnerPlurality,
               prop_MajorityCriteria winnerOne,
               prop_MajorityCriteria winnerPlurality
               ]
