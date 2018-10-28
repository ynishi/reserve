{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
  ( someFunc
  , Reservation(..)
  , State(..)
  , URL
  , DoAt
  , eval
  , toList
  , fromList
  , execute
  , empty
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Fixed
import           Data.List              ((\\))
import           Data.Time
import           Data.Time.Clock

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type DoAt = UTCTime

type URL = String

data State
  = New
  | Canceled
  | Waiting
  | Doing
  | DoneState
  | Errored
  deriving (Show, Eq, Ord)

data Reservation a b c where
  Empty :: Reservation a b c
  Reservation :: URL -> DoAt -> State -> Reservation URL DoAt State
  After
    :: NominalDiffTime
    -> Reservation a b c
    -> Reservation a b c
    -> Reservation a b c
  Append :: Reservation a b c -> Reservation a b c -> Reservation a b c
  Disable :: Ord b => b -> b -> Reservation a b c -> Reservation a b c
  DisableAfter :: Ord b => b -> Reservation a b c -> Reservation a b c
  Remove :: Reservation a b c -> Reservation a b c -> Reservation a b c
  Done :: Reservation a b c -> Reservation a b c

deriving instance
         Show (Reservation URL DoAt State) =>
         Show (Reservation URL DoAt State)

eval :: Ord b => forall a. Reservation URL b State -> [(URL, b, State)]
eval Empty = []
eval (Reservation url ut s) = [(url, ut, s)]
eval (After sec r1@(Reservation _ at _) (Reservation url _ state)) =
  eval r1 ++ eval (Reservation url (addUTCTime sec at) state)
eval (Append r1 r2) = eval r1 ++ eval r2
eval (Disable t1 t2 r1) = filter (\(_, ut, _) -> ut < t1 || ut > t2) $ eval r1
eval (DisableAfter t r) = filter (\(_, ut, _) -> ut < t) $ eval r
eval (Done r@(Reservation _ _ c)) =
  map (\(a, b, _) -> (a, b, DoneState)) $ eval r
eval (Remove r1 r2) = eval r1 \\ eval r2

toList :: Reservation URL DoAt State -> [(URL, DoAt, State)]
toList = eval

fromList :: [(URL, DoAt, State)] -> Reservation URL DoAt State
fromList [] = empty
fromList xs = foldl1 Append ts
  where
    ts = map fromTuple xs

fromTuple :: (URL, DoAt, State) -> Reservation URL DoAt State
fromTuple (url, doAt, state) = Reservation url doAt state

empty :: Reservation a b c
empty = Empty

execute :: MonadIO m => Reservation URL DoAt State -> m ()
execute r = do
  now <- liftIO getCurrentTime
  let dar = DisableAfter now r
  liftIO . print $ show $ eval dar
