{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Prelude hiding (FilePath)
import Shelly
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Control.Monad
default (Text)

data Repo = Repo { repoName :: Text
                 , repoURL :: Text }
          deriving (Eq, Ord, Show)

repos :: [Repo]
repos = [ Repo "baysig-core"  "git@github.com:glutamate/baysig-core.git"
        , Repo "baysig-exec"  "git@github.com:glutamate/baysig-exec.git"
        , Repo "bugsess"      "git@github.com:glutamate/bugsess.git"
        ]

git = command1_ "git" []

mergeDir = "merge"

makeMergeDir :: Sh ()
makeMergeDir = do
  echo "\nCREATING WORKING DIRECTORY\n"
  rm_rf mergeDir
  mkdir mergeDir

preProcessRepo :: Repo -> Sh ()
preProcessRepo (Repo name url) = do
  echo $ "\nPRE-PROCESSING REPOSITORY: " <> name <> "\n"
  git "clone" [url, name <> "_copy"]
  cd $ fromText $ name <> "_copy"
  let filtercmd = "if [[ ! -e " <> name <> " ]]; then " <>
                  "mkdir " <> name <> " ; " <>
                  "git ls-tree --name-only $GIT_COMMIT | " <>
                  "xargs -I files mv files " <> name <> " ; " <>
                  "fi"
  git "filter-branch" ["--prune-empty", "--tree-filter", filtercmd]
  cd ".."

combineRepos :: [Repo] -> Sh ()
combineRepos repos = do
  echo "\nMERGING REPOSITORIES\n"
  git "clone" [repoName (head repos) <> "_copy", "merged"]
  cd "merged"
  forM_ (tail repos) $ \(Repo name url) -> do
    git "remote" ["add", "-f", name, "../" <> name <> "_copy"]
    git "merge" ["-s", "ours", "--no-commit", name <> "/master"]
    git "read-tree" ["--prefix=/", "-u", name <> "/master"]
    git "commit" ["-m", "Merged " <> name]
    git "pull" ["-s", "subtree", name, "master"]

main :: IO ()
main = shellyNoDir $ verbosely $ do
  makeMergeDir
  cd mergeDir
  forM_ repos preProcessRepo
  combineRepos repos
  echo "DONE"
