module LockerMap
( LockerMap
, LockerState
, Code
, LockerNumber
, lockerLookup
, exampleLockers
) where

import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerNumber = Int

type LockerMap = Map.Map LockerNumber (LockerState, Code)

lockerLookup :: LockerNumber -> LockerMap -> Either String Code
lockerLookup lockerNumber lockerMap = case Map.lookup lockerNumber lockerMap of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
      then Right code
      else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

exampleLockers :: LockerMap
exampleLockers = Map.fromList
    [(100, (Taken, "ASDF"))
    ,(101, (Free, "SDFG"))
    ,(102, (Free, "QWQE"))
    ,(103, (Taken, "OIJJ"))
    ,(104, (Free, "OUQE"))
    ,(105, (Free, "OMDJ"))
    ,(106, (Taken, "QWEU"))
    ]