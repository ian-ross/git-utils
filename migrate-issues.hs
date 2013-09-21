{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forM)
import Data.Function (on)
import Data.List (sortBy, isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit
import System.Environment
import Github.Repos
import Github.Issues
import Github.Issues.Comments

-- | You need to supply an appropriate GitHub user name and password
-- here.  This user needs to have write access to the repository you
-- want to import issues into.
ghAuth :: GithubAuth
ghAuth = GithubBasicAuth "ian-ross" "<<PASSWORD REDACTED>>"

-- | The repository you want to import issues into: the user (possibly
-- an organisation name) is the ownder of the repo (not necessarily
-- the same as the authorisation user name).
ghUser, ghRepo :: String
ghUser = "glutamate"
ghRepo = "baysig-platform"

ghRepoUser :: String
ghRepoUser = "glutamate"

-- | The repositories you want to export issues from.
repos :: [String]
repos = ["baysig-core", "baysig-exec", "bugsess"]



migrateIssue :: Int -> String -> Issue -> [IssueComment] -> IO ()
migrateIssue num r Issue{..} cs = do
  -- Basic issue creation: assignee + labels
  let labels = Just $ r : map labelName issueLabels
      newi = NewIssue issueTitle
                      issueBody
                      (githubOwnerLogin <$> issueAssignee)
                      (milestoneNumber <$> issueMilestone)
                      labels
      closed = case issueClosedAt of
        Nothing -> False
        Just _ -> True
  res <- wrap $ createIssue ghAuth ghUser ghRepo newi
  case res of
    Left err -> do
      putStrLn $ "Failed to create issue"
      putStrLn $ show err
    Right Issue{..} -> do
      putStrLn $ "Issue #" ++ show issueNumber ++ " created"
      forM_ cs (addComment r issueNumber)
      -- Closed?  Yes: change state
      if closed
        then closeIssue issueNumber
        else putStrLn "Issue still open..."

closeIssue :: Int -> IO ()
closeIssue iss = do
  let edit = editOfIssue { editIssueState = Just "closed" }
  r <- wrap $ editIssue ghAuth ghUser ghRepo iss edit
  case r of
    Left err -> do
      putStrLn $ "Failed to close issue #" ++ show iss
      putStrLn $ show err
    Right _ -> putStrLn $ "Closed issue #" ++ show iss

addComment :: String -> Int -> IssueComment -> IO ()
addComment r iss comm = do
  r <- wrap $ createComment ghAuth ghUser ghRepo iss (issueCommentBody comm)
  case r of
    Left err -> do
      putStrLn $ "Failed to create comment for issue #" ++ show iss
      putStrLn $ show err
    Right _ -> putStrLn $ "Comment created for issue #" ++ show iss

wrap :: IO (Either Error a) -> IO (Either Error a)
wrap act = go (10 :: Integer) where
  go n = do
    res <- act
    case res of
      Right _ -> return res
      Left err ->
        if n > 0
        then do
          putStrLn $ "Error: " ++ show err ++ " -- retrying in 5 s"
          threadDelay 5000000
          go $ n - 1
        else do
          putStrLn $ "Error: " ++ show err ++ " -- too many retries!"
          return res


collectIssues :: String -> IO [(String, Issue, [IssueComment])]
collectIssues r = do
  putStrLn $ "Collecting issues for " ++ ghRepoUser ++ "/" ++ r
  eopen <- wrap $ issuesForRepo' (Just ghAuth) ghRepoUser r []
  eclosed <- wrap $ issuesForRepo' (Just ghAuth) ghRepoUser r [OnlyClosed]
  case (eopen, eclosed) of
    (Right open, Right closed) -> do
      let allissues =
            filter ((r `notElem`) . map labelName . issueLabels) $
            open ++ closed
      forM allissues $ \i -> do
        putStrLn $ "Assignee: " ++ show (githubOwnerLogin <$> issueAssignee i)
        putStrLn $ "Requesting comments for issue " ++ show (issueNumber i)
        ecs <- wrap $ comments' (Just ghAuth) ghRepoUser r (issueNumber i)
        cs <- case ecs of
          Left err -> do
            putStrLn $ "Failed to get comments for issue: " ++
              show (issueNumber i) ++ ", repository " ++ r
            putStrLn $ show err
            exitFailure
          Right csok -> return csok
        return (r, i, cs)
    _ -> do
      putStrLn $ "Failed to get issues for repository " ++ r
      putStrLn $ show eopen
      putStrLn $ show eclosed
      exitFailure

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["collect"] -> do
      allissues <- sortBy (compare `on` (issueCreatedAt . snd3)) . concat <$>
                   mapM collectIssues repos
      TIO.writeFile "all-issues.dat" (T.pack $ show allissues)
    ["create"] -> do
      fcont <- T.unpack <$> TIO.readFile "all-issues.dat"
      let allissues = (read fcont) :: [(String, Issue, [IssueComment])]
      let renumbered =
            zipWith (\num (r, i, cs) -> (num, r, i, cs)) [1..] allissues
      forM_ renumbered (\(num, n, i, cs) -> do
                           putStrLn $ show num ++ ": " ++ n ++ " " ++
                                      show (issueNumber i) ++ " " ++
                                      issueTitle i
                           migrateIssue num n i cs)
    _ -> putStrLn "Invalid arguments!"
