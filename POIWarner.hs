{-
 - ----------------------------------------------------------------------------
 - "THE BEER-WARE LICENSE" (Revision 42):
 - <code@gregorkopf.de> wrote this file. As long as you retain this notice you
 - can do whatever you want with this stuff. If we meet some day, and you
 - think this stuff is worth it, you can buy me a beer in return. Gregor Kopf
 - ----------------------------------------------------------------------------
 -}

{-| A GPS based POI warner -}
module Main(main) where

import           Network.Gpsd
import           System.Environment
import           System.IO
import           System.Exit
import           Data.Csv
import           Control.Monad
import           Control.Monad.Loops
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import           Control.Applicative
import           Data.List
import           Data.Maybe
import           System.Process
import           Data.String.Utils
import           Control.Exception

-- | A POI
data POI = POI {
      poiPosition     :: GPSPosition
    , poiDescription  :: String
    , poiLastDistance :: Double
    , poiAnnouncedAt  :: Int
   } deriving (Show)

instance Eq POI where
   poi1 == poi2 = poiPosition poi1 == poiPosition poi2 && 
                   poiDescription poi1 == poiDescription poi2

-- | Creates a POI from a position and a description
mkPOI :: GPSPosition -> String -> POI
mkPOI pos desc = POI { 
    poiPosition     = pos
  , poiDescription  = desc
  , poiLastDistance = radiusHotPOIs
  , poiAnnouncedAt  = radiusHotPOIs
 }

-- | How far may a POI be so that we preload it from a CSV for further
-- processing?
radiusHotPOIs :: Double
radiusHotPOIs = 10000

pos :: GPSContext -> IO GPSPosition
pos ctx = fromJust <$> getPosition ctx

speed :: GPSContext -> IO Double
speed ctx = fromJust <$> getSpeed ctx

-- | Calculates the distance (in meters) between to GPSPositions.
gpsDistance :: GPSPosition -> GPSPosition -> Double
gpsDistance pos1 pos2 =
    let long1 = gpsLongitude pos1
        lat1  = gpsLatitude  pos1
        long2 = gpsLongitude pos2
        lat2  = gpsLatitude  pos2
        d2r = pi/180
        dlong = (long2 - long1) * d2r
        dlat = (lat2 - lat1) * d2r
        a = (sin(dlat/2.0) ** 2) + cos(lat1*d2r) * cos(lat2*d2r) * (sin(dlong/2.0) ** 2)
        c = 2 * atan2 (sqrt a) (sqrt (1-a))
    in 6367000 * c;

-- | Takes care of regularly reloading a POI database.
databaseUpdateThread :: String -> GPSContext -> Double -> TVar [POI] -> IO ()
databaseUpdateThread filename ctx distance tv = forever $ do
   curPos <- pos ctx
   result <- try $ scanDatabase filename curPos distance tv :: IO (Either SomeException ())
   case result of
      Left err -> hPutStrLn stderr $ show err
      Right _  -> return ()
   threadDelay $ 1000000 * 60 -- Updating once a minute is enough.

-- | Scans an IGO8 database and appends all POIs found to a list of POIs in 
-- a TVar.
scanDatabase :: String -> GPSPosition -> Double -> TVar [POI] -> IO ()
scanDatabase filename curpos distance tv = do
   handle <- openFile filename ReadMode
   hGetLine handle -- Skip header
   whileM (not <$> hIsEOF handle) $ do
      line <- BL.fromStrict <$> C8.hGetLine handle
      let dec = decode NoHeader line 
      case dec of
         Left err -> return () -- Should we do something here?
         Right v  -> if V.length v == 0
                        then return ()
                        else do let e = processEntry curpos distance (V.head v)
                                case e of
                                  Nothing -> return ()
                                  Just p  -> atomically $ do
                                                lst <- readTVar tv
                                                if p `elem` lst
                                                   then return ()
                                                   else writeTVar tv (p:lst)
   hClose handle

-- | Generates a POI if the object is within the provided distance
processEntry ::    GPSPosition 
                -> Double 
                -> (Double, Double, String, String, String, String) 
                -> Maybe POI
processEntry curpos distance (lon, lat, typ, _, _, _) =
   let posPoi = GPSPosition { gpsLongitude = lon, gpsLatitude = lat }
   in if (gpsDistance curpos posPoi) <= distance
         then Just $ mkPOI posPoi typ
         else Nothing

-- | Gets a list of POIs that are still interesting to us (i.e., within
-- the given distance).
updateHotPOIs :: GPSPosition -> Double -> [POI] -> [POI]
updateHotPOIs curPos distance =
   filter ((<= distance) . gpsDistance curPos . poiPosition)

-- | Sorts a list of POIs, based on their distance to the given position.
sortPOIs :: GPSPosition -> [POI] -> [POI]
sortPOIs curPos =
   sortBy (\p1 p2 -> compare (gpsDistance curPos (poiPosition p1))
                             (gpsDistance curPos (poiPosition p2)))

-- | The actual POI warning logic. It will find the nearest POI and pass
-- it to the POI handling function once every second.
poiWarnerThread :: TVar [POI] -> GPSContext -> Double -> String -> IO ()
poiWarnerThread tv ctx radiusHot commandTemplate = forever $ do
   curPos <- pos ctx
   next   <- atomically $ do
      modifyTVar tv (sortPOIs curPos . updateHotPOIs curPos radiusHot)
      lst <- readTVar tv
      return $ takeWhile ((<= 1000) . gpsDistance curPos . poiPosition) lst
   forM_ next $ \poi -> do
      newPOI <- handleOnePOI ctx poi commandTemplate
      changeOne tv newPOI
   threadDelay $ 1000000
   where changeOne tv poi = atomically $ modifyTVar tv (\l -> poi:(filter (/=poi) l))

-- | Handles one POI
handleOnePOI :: GPSContext -> POI -> String -> IO POI
handleOnePOI ctx poi commandTemplate = do
   curPos <- pos ctx
   let distance = gpsDistance curPos (poiPosition poi)
   let newPoi = poi { poiLastDistance = distance }
   if (poiLastDistance poi) >= distance
      then announce newPoi ctx commandTemplate
      else return newPoi { poiAnnouncedAt = round distance }

-- | Checks the last distance of a POI and announces it if needed.
announce :: POI -> GPSContext -> String -> IO POI
announce poi ctx commandTemplate = do
   let distance = poiLastDistance poi
   spd <- speed ctx
   if (distance <= 1000 && spd >= 22) 
      then doAnnouncement poi 1000 commandTemplate
      else if (distance <= 500) 
            then doAnnouncement poi 500 commandTemplate
            else if (distance <= 100) 
                  then doAnnouncement poi 100 commandTemplate
                  else return poi

-- | Checks if the poi has already be announced and if it hasn't then
-- we issue an announcement.
doAnnouncement :: POI -> Int -> String -> IO POI
doAnnouncement poi announceDist commandTemplate = 
   if poiAnnouncedAt poi > announceDist
      then do outputWarning poi announceDist commandTemplate
              return poi { poiAnnouncedAt = announceDist }
      else return poi

-- | Performs the necessary IO for issuing a POI warning
outputWarning :: POI -> Int -> String -> IO ()
outputWarning poi distance commandTemplate = do
   let cmd' = replace "%d" (show distance) commandTemplate 
       cmd  = replace "%s" (poiDescription poi) cmd'
   _ <- try $ callCommand cmd  :: IO (Either SomeException ())
   return ()

main :: IO ()
main = do
   args <- getArgs
   when (length args < 4) $ do
      hPutStrLn stderr "Usage: POIWarner gpsd_host gpsd_port warn_command database1 ..."
      exitFailure
   gps <- initGps (args !! 0) (read $ args !! 1)
   waitForFix gps
   tv <- newTVarIO []
   forM_ (drop 3 args) $ \filename -> do
      forkIO $ databaseUpdateThread filename gps radiusHotPOIs tv
   poiWarnerThread tv gps radiusHotPOIs (args !! 2)
