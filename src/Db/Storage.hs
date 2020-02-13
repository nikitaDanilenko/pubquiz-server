module Db.Storage where

import           Data.Time.Calendar (Day)
import           Db.Connection      (DbQuizId)
import           Db.DbTypes         (Activity, Code, Place, QuizName,
                                     RoundNumber, TeamName, TeamNumber)
import           Labels             (Labels)

setTeamRoundPoints :: DbQuizId -> RoundNumber -> TeamNumber -> Double -> IO ()
setTeamRoundPoints = undefined

setReachable :: DbQuizId -> RoundNumber -> Double -> IO ()
setReachable = undefined

setTeam :: DbQuizId -> TeamNumber -> Code -> TeamName -> Activity -> IO ()
setTeam = undefined

setLabels :: DbQuizId -> Labels -> IO ()
setLabels = undefined

createQuiz :: Place -> Day -> QuizName -> IO ()
createQuiz = undefined

lockQuiz :: DbQuizId -> IO ()
lockQuiz = undefined

findAllPointsForQuiz :: DbQuizId -> IO ()
findAllPointsForQuiz = undefined
