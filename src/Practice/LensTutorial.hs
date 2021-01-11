{-# LANGUAGE TemplateHaskell #-}

module Practice.LensTutorial where

import Control.Lens

-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial

type Degrees = Double

type Latitude = Degrees

type Longitude = Degrees

data Meetup = Meetup {_name :: String, _location :: (Latitude, Longitude)} deriving (Show, Eq)

makeLenses ''Meetup
