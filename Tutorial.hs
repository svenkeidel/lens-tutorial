
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import           Data.Functor.Identity
import           Data.Functor.Constant
import           Data.Monoid
import qualified Data.Map as M
import qualified Control.Lens as L
import qualified Control.Lens.Type as L
import           Control.Lens.Operators

data Meetup = Meetup
    { name :: String
    , location :: Location
    } deriving (Show)

data Location = Location
    { latitude :: Latitude
    , longitude :: Longitude
    } deriving (Show)

newtype Latitude = Latitude { getLatitude :: Double } deriving (Show,Num)
newtype Longitude = Longitude { getLongitude :: Double } deriving (Show,Num)

hug :: Meetup
hug = Meetup
 { name = "Frankfurt Haskell Usergroup"
 , location = Location
     { latitude = Latitude 50.1262
     , longitude = Longitude 8.707792
     }
 }
 

mapLongitudeOfMeetup :: (Longitude -> Longitude) -> (Meetup -> Meetup)
mapLongitudeOfMeetup f meetup =
    meetup { location =
              (location meetup) { longitude = 
                  f (longitude (location meetup)) } }

mapLongitudeOfMeetup (+1) hug

mapLocation :: (Location -> Location) -> (Meetup -> Meetup)
mapLocation f meetup =
    meetup { location = f (location meetup) }

mapLongitude :: (Longitude -> Longitude) -> (Location -> Location)
mapLongitude f loc = loc { longitude = f (longitude loc) }

compose f meetup = mapLocation (mapLongitude f) meetup
:t compose

compose (+1) hug

(mapLocation . mapLongitude) (+1) hug

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
--
-- Specialized for (mapLocation . mapLongitude)
-- (.) :: ((Location -> Location) -> (Meetup -> Meetup))
--     -> ((Longitude -> Longitude) -> (Location -> Location))
--     -> ((Longitude -> Longitude) -> (Meetup -> Meetup))
:t fmap

mapLocation' :: Functor f => (Location -> f Location) -> (Meetup -> f Meetup)
mapLocation' g meetup = fmap (\loc -> meetup { location = loc }) (g (location meetup))

mapLongitude' :: Functor f => (Longitude -> f Longitude) -> (Location -> f Location)
mapLongitude' f loc = fmap (\long -> loc { longitude = long }) (f (longitude loc))

:t (mapLocation' . mapLongitude')

(mapLocation' . mapLongitude') (\l -> print l >> return (l+1)) hug

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
type Lens' s a = Lens s s a a

type Setter s t a b = (a -> Identity b) -> s -> Identity t

-- set :: Setter' Meetup Location -> Location -> Meetup -> Meetup
set :: Setter s t a b -> b -> s -> t
set setter b s = runIdentity $ setter (\_ -> Identity b) s 

set (mapLocation' . mapLongitude') 42 hug

set (L.mapped . mapLocation' . mapLongitude') 42 (Just hug)

hug & (mapLocation' . mapLongitude') +~ 1
hug & (mapLocation' . mapLongitude') -~ 1
hug & (mapLocation' . mapLongitude') *~ 2

type Getting r s a = (a -> Constant r a) -> (s -> Constant r s)

get :: Getting a s a -> s -> a
get getter s = getConstant $ getter (\a -> Constant a) s


get (mapLocation' . mapLongitude') hug
get (mapLocation' . mapLongitude' . L.to getLongitude) hug

get (L.at 2) (M.fromList [(1,'a'), (2,'b'), (3,'c')])
set (L.at 2) (Just 'c') (M.fromList [(1,'a'), (2,'b'), (3,'c')])
L.at 2 ?~ 'c' $ M.fromList [(1,'a'), (2,'b'), (3,'c')]

mapLongLat :: Applicative f => (Double -> f Double) -> (Location -> f Location)
mapLongLat f loc = Location <$> fmap Latitude (f (getLatitude (latitude loc)))
                            <*> fmap Longitude (f (getLongitude (longitude loc)))

set (mapLocation' . mapLongLat) 4 hug
get (mapLocation' . mapLongLat) hug

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf lens f s = getConstant $ lens (Constant . f) s

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo . f)

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []

toListOf (mapLocation' . mapLongLat) hug

anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
anyOf l f = getAny . foldMapOf l (Any . f)

allOf :: Getting All s a -> (a -> Bool) -> s -> Bool
allOf l f = getAll . foldMapOf l (All . f)

anyOf (mapLocation' . mapLongLat) (<= 8) hug
anyOf (mapLocation' . mapLongLat) (<= 9) hug
allOf (mapLocation' . mapLongLat) (>= 8) hug
