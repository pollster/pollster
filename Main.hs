{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings #-}
import Debug.Trace
import Data.Function
import Data.Ord
import Database.Persist
import Database.Persist.Base
import Database.Persist.TH
import Database.Persist.Postgresql
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import InstantRunOff
import Data.List(groupBy, sortBy)

connstr = "user=postgres password= host=localhost port=5432 dbname=metoo"

share [mkPersist, mkMigrate "migrateAll"] [$persist|
BallotSheet
    name String Gt
Candidate
    name String Gt
Vote
    ballotSheetId BallotSheetId
    candidateId CandidateId
    rank Int Gt
|]
main = withPostgresqlPool connstr 1 $ runSqlPool  $ do
    runMigration migrateAll
    deleteWhere [VoteRankGt 0]
    deleteWhere [BallotSheetNameGt ""]
    deleteWhere [CandidateNameGt ""]
    candidateIds <- mapM (insert . Candidate) ["Machan", "Gavri", "Another"]
    ballotSheetIds <- mapM (insert . BallotSheet) ["b1", "b2", "b3", "b4", "b5"]
    voteIds <- mapM (\(bid, cid, rank) -> insert (Vote (ballotSheetIds !! bid) (candidateIds !! cid) rank)) createTestData
    votes <- selectList [VoteRankGt 0] [] 0 0
    let voteValues = map snd votes
    liftIO $ print $ winnerOne $ map2 voteCandidateId $ groupBy (\left right -> voteBallotSheetId left == voteBallotSheetId right) $ sortBy (comparing voteBallotSheetId) voteValues
    return ()
vm = [
 [1, 3, 1, 1, 2],
 [2, 2, 2, 3, 1],
 [3, 1, 3, 2, 1]
 ]
createTestData = [(bid, cid, rank) | bid <- [0..4], cid <- [0..2], rank <- [1..3],  ((vm !! (rank -1)) !! bid) == (cid + 1)]

map2 :: (a -> b)-> [[a]] -> [[b]]
map2 f = map (map f)


