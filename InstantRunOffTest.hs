import InstantRunOff
import Test.QuickCheck
import Control.Monad(join)
import Control.Monad.Trans
import Control.Monad
import Data.List

prop_Simple = forAll ballotSheetsSimple $ (\xss -> (((length . join) xss) /= 0) ==> winner losersOneAtATime xss `elem` join xss)

ballotSheetsSimple :: Gen [[Int]]
ballotSheetsSimple = do
      res <- arbitrary
      return $ (fmap $ (nub . (fmap (`mod` 5)))) res

main = do
      quickCheck prop_Simple
