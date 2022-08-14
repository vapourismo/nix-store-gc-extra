{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Cleff (Eff, Effect, type (:>), type (:>>))
import Cleff qualified
import Cleff.State qualified as State
import Control.Monad (when)
import Data.DList qualified as DList
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Time.Clock qualified as Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Traversable (for)
import Nix.Store.GC.Extra.Paths qualified as Paths
import Options.Applicative qualified as Options
import System.Posix.Files qualified as Files
import System.Process qualified as Process

data Entry = Entry
  { root :: FilePath
  -- ^ Root path of the Nix store entry
  , lastTime :: Clock.UTCTime
  -- ^ Last time something happened to that path
  }
  deriving stock (Show, Eq, Ord)

-- | Invoke @nix-store@ to find out which paths are dead.
listDeadPaths :: IO [FilePath]
listDeadPaths = do
  output <- Process.readProcess Paths.nixStore ["--gc", "--print-dead"] ""
  paths <- for (lines output) \path -> do
    exists <- Files.fileExist path
    if exists
      then pure (Just path)
      else pure Nothing

  pure (catMaybes paths)

-- | Keeps track of an ordered list of paths that shall be deleted.
data DeletionOrder = DeletionOrder
  { paths :: DList.DList FilePath
  , pathSet :: HashSet.HashSet FilePath
  }

instance Semigroup DeletionOrder where
  lhs <> rhs =
    DeletionOrder
      { paths = DList.append lhs.paths rhs.paths
      , pathSet = HashSet.union lhs.pathSet rhs.pathSet
      }

instance Monoid DeletionOrder where
  mempty = DeletionOrder {paths = DList.empty, pathSet = HashSet.empty}

mkDeletionOrder :: FilePath -> DeletionOrder
mkDeletionOrder path =
  DeletionOrder
    { paths = DList.singleton path
    , pathSet = HashSet.singleton path
    }

data NixStoreCache = NixStoreCache
  { alreadyDeleted :: DeletionOrder
  , referrers :: HashMap.HashMap FilePath [FilePath]
  }

data NixStore :: Effect where
  DeleteStorePaths :: [FilePath] -> NixStore f ()
  GetReferrers :: FilePath -> NixStore f [FilePath]

$(Cleff.makeEffect ''NixStore)

interpretNixStore :: Cleff.IOE :> es => Eff (NixStore : es) a -> Eff es a
interpretNixStore program = do
  (result, state) <-
    State.runState initState $
      Cleff.reinterpret
        \case
          DeleteStorePaths paths -> do
            State.state @NixStoreCache \state ->
              let toDelete =
                    filter
                      (\path -> not (HashSet.member path state.alreadyDeleted.pathSet))
                      paths
               in ( ()
                  , state
                      { alreadyDeleted =
                          mconcat (state.alreadyDeleted : map mkDeletionOrder toDelete)
                      }
                  )
          GetReferrers path -> do
            state <- State.get @NixStoreCache
            case HashMap.lookup path state.referrers of
              Just referrers -> pure referrers
              Nothing -> do
                refs <- Cleff.liftIO (runListReferrers path)
                State.modify \state ->
                  state {referrers = HashMap.insert path refs state.referrers}
                pure refs
        program

  Cleff.liftIO $ runDeleteChunked $ DList.toList state.alreadyDeleted.paths

  pure result
  where
    initState = NixStoreCache {alreadyDeleted = mempty, referrers = HashMap.empty}

    runDelete paths = do
      for_ paths print
      Process.callProcess
        Paths.nixStore
        ("--delete" : "--ignore-liveness" : paths)

    runDeleteChunked [] = pure ()
    runDeleteChunked paths = do
      let (head, tail) = List.splitAt 1000 paths
      runDelete head
      for_ head print
      runDeleteChunked tail

    runListReferrers topPath = do
      output <- Process.readProcess Paths.nixStore ["--query", "--referrers-closure", topPath] ""
      paths <- for (lines output) \path -> do
        exists <- Files.fileExist path
        if exists && path /= topPath
          then pure (Just path)
          else pure Nothing

      pure (catMaybes paths)

data Age :: Effect where
  IsOldEnough :: FilePath -> Age f Bool

$(Cleff.makeEffect ''Age)

interpretAge :: Cleff.IOE :> es => Clock.NominalDiffTime -> Eff (Age : es) b -> Eff es b
interpretAge maxAge program = do
  time <- Cleff.liftIO Clock.getCurrentTime
  Cleff.interpret
    \case
      IsOldEnough path -> do
        exists <- Cleff.liftIO (Files.fileExist path)
        if exists
          then do
            status <- Cleff.liftIO (Files.getFileStatus path)

            let atime = posixSecondsToUTCTime (Files.accessTimeHiRes status)
            let mtime = posixSecondsToUTCTime (Files.modificationTimeHiRes status)
            let ctime = posixSecondsToUTCTime (Files.statusChangeTimeHiRes status)
            let lastTime = maximum [atime, mtime, ctime]

            pure (Clock.diffUTCTime time lastTime > maxAge)
          else pure False
    program

deleteRefs ::
  [NixStore, Age, State.State (HashSet.HashSet FilePath)] :>> es =>
  FilePath ->
  Eff es Bool
deleteRefs path = do
  alreadySeen <- State.state @(HashSet.HashSet FilePath) \paths ->
    ( HashSet.member path paths
    , HashSet.insert path paths
    )
  if not alreadySeen
    then do
      mayDelete <- isOldEnough path
      if mayDelete
        then do
          refs <- getReferrers path
          dels <- for refs deleteRefs
          if and dels then True <$ deleteStorePaths refs else pure False
        else pure False
    else pure True

program ::
  [NixStore, Age, State.State (HashSet.HashSet FilePath)] :>> es =>
  FilePath ->
  Eff es ()
program path = do
  dels <- deleteRefs path
  when dels do
    _ <- deleteStorePaths [path]
    pure ()

data Options = Options
  {maxAge :: Clock.NominalDiffTime}
  deriving stock (Show, Eq, Ord)

optionsInfo :: Options.ParserInfo Options
optionsInfo =
  Options.info (Options.helper <*> parser) mempty
  where
    parser = Options <$> ageOption

    ageOption =
      fmap fromInteger $
        Options.option Options.auto $
          mconcat
            [ Options.long "delete-older-than"
            , Options.short 'd'
            , Options.help "Delete paths older than this (age in seconds)"
            ]

main :: IO ()
main = do
  options <- Options.execParser optionsInfo
  paths <- listDeadPaths
  _ <-
    Cleff.runIOE $
      State.runState (mempty @(HashSet.HashSet FilePath)) $
        interpretAge options.maxAge $
          interpretNixStore $
            for_ paths program
  pure ()
