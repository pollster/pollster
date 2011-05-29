import InstantRunOff
import Test.QuickCheck
import Control.Monad(join)
import Control.Monad.Trans
import Control.Monad
import Data.List

prop_Simple = forAll ballotSheetsSimple $ (\xss -> (not . null . join) xss ==> winner losersOneAtATime xss `elem` join xss)
prop_MajorityCriteria = forAll ballotSheetsMajorityCriteria $ (\xss -> (not . null . join) xss  ==> winner losersOneAtATime (map ((:[]) . head) (filter (not . null) xss)) == winner losersOneAtATime xss)

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
      quickCheck prop_Simple
      quickCheck prop_MajorityCriteria
