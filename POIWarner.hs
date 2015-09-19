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
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import           Control.Applicative
import           Data.List
import           Data.Maybe
import           System.Cmd
import           Data.String.Utils
import           Control.Exception
import           Database.SQLite.Simple
import           System.Log.Logger
import           System.Log.Handler.Syslog
import           System.Log.Handler.Simple
import           System.Log.Formatter
import           System.Log.Handler (setFormatter)

myName :: String
myName = "HsPOI"

debug :: String -> IO ()
debug = debugM myName

warning :: String -> IO ()
warning = warningM myName

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
  , poiAnnouncedAt  = round $ radiusHotPOIs
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

-- | Computes a rough approximation of a bounding box around a given
-- GPSPosition.
gpsBoundingBox :: GPSPosition -> Double -> (GPSPosition, GPSPosition)
gpsBoundingBox pos dist =
   let latOffset = dist / 110574
       lonOffset = dist / abs (111320 * cos((gpsLatitude pos) * (pi/180)))
   in (pos {gpsLatitude = gpsLatitude pos - latOffset, gpsLongitude = gpsLongitude pos - lonOffset},
       pos {gpsLatitude = gpsLatitude pos + latOffset, gpsLongitude = gpsLongitude pos + lonOffset})

-- | Takes care of regularly reloading a POI database.
databaseUpdateThread :: DBType -> GPSContext -> Double -> TVar [POI] -> IO ()
databaseUpdateThread db ctx distance tv = forever $ do
   debug "Updating the database."
   curPos <- pos ctx
   result <- try $ scanDB db curPos distance tv :: IO (Either SomeException ())
   case result of
      Left err -> warning $ "Database scan failed: " ++ show err
      Right _  -> return ()
   threadDelay $ 1000000 * 60 -- Updating once a minute is enough.
   where scanDB (CSV filename) curPos distance tv = 
            scanDatabase filename curPos distance tv
         scanDB (SQLITE3 filename fields) curPos distance tv = 
            scanDatabaseSqlite filename curPos distance tv fields

-- | Scans an IGO8 database and appends all POIs found to a list of POIs in 
-- a TVar.
scanDatabase :: String -> GPSPosition -> Double -> TVar [POI] -> IO ()
scanDatabase filename curpos distance tv = do
   handle <- openFile filename ReadMode
   hGetLine handle -- Skip header
   whileM (not <$> hIsEOF handle) $ do
      line <- (BL.pack . BS.unpack) <$> C8.hGetLine handle
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

-- | The representation of a POI in a SQLITE3 database
instance FromRow POI where
   fromRow = mkPOI <$> (GPSPosition <$> field <*> field) <*> field

-- | Scans a SQLITE database and appends all POIs found to a list of POIs in
-- a TVar.
scanDatabaseSqlite :: String -> GPSPosition -> Double -> TVar [POI] -> (String, String, String, String, Maybe String) -> IO ()
scanDatabaseSqlite filename curpos distance tv (tblName, latField, lonField, typeField, mwc) = do
  conn <- open filename
  let (small, big) = gpsBoundingBox curpos distance
  let minLat = gpsLatitude small
  let minLon = gpsLongitude small
  let maxLat = gpsLatitude big
  let maxLon = gpsLongitude big
  let whereClause = maybe "" (\x -> " AND (" ++ x ++ ")") mwc
  let q = "SELECT " ++ lonField ++ ", " ++ latField ++ ", " ++ typeField ++ 
          " FROM " ++ tblName ++ " WHERE " ++ latField ++ " BETWEEN " ++ 
          (show minLat) ++ " AND " ++ (show maxLat) ++ " AND " ++ lonField ++ 
          " BETWEEN " ++ show (minLon) ++ " AND " ++ (show maxLon) ++ whereClause
  debug $ "SQLite query: " ++ (show q)
  r <- query_ conn (read $ show q) :: IO [POI]
  debug $ "Got " ++ (show $ length r) ++ " rows."
  close conn
  atomically $ modifyTVar tv $ (flip union) r

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
   debug $ "Current GPS position: " ++ show curPos
   next   <- atomically $ do
      modifyTVar tv (sortPOIs curPos . updateHotPOIs curPos radiusHot)
      lst <- readTVar tv
      return $ takeWhile ((<= 1000) . gpsDistance curPos . poiPosition) lst
   forM_ next $ \poi -> do
      debug $ "Processing POI " ++ show poi
      newPOI <- handleOnePOI ctx poi commandTemplate
      debug $ "After processing we have " ++ show newPOI
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
      else do debug $ "Driving away from POI " ++ (show newPoi) ++ 
                      ". New distance is " ++ show (round distance)
              return newPoi { poiAnnouncedAt = round distance }

-- | Checks the last distance of a POI and announces it if needed.
announce :: POI -> GPSContext -> String -> IO POI
announce poi ctx commandTemplate = do
   let distance = poiLastDistance poi
   spd <- speed ctx
   if distance <= 100
      then doAnnouncement poi 100 commandTemplate
      else if (distance <= 300) 
            then doAnnouncement poi 300 commandTemplate
            else if (distance <= 500) 
                  then doAnnouncement poi 500 commandTemplate
                  else if (distance <= 1000 && spd >= 22)
                        then doAnnouncement poi 1000 commandTemplate
                        else return poi

-- | Checks if the poi has already be announced and if it hasn't then
-- we issue an announcement.
doAnnouncement :: POI -> Int -> String -> IO POI
doAnnouncement poi announceDist commandTemplate = 
   if poiAnnouncedAt poi > announceDist
      then do debug $ "Will announce POI " ++ (show poi) ++ " now."
              outputWarning poi announceDist commandTemplate
              return poi { poiAnnouncedAt = announceDist }
      else do debug $ "Skipping double announcement of POI " ++ show poi
              return poi

-- | Performs the necessary IO for issuing a POI warning
outputWarning :: POI -> Int -> String -> IO ()
outputWarning poi distance commandTemplate = do
   let cmd' = replace "%d" (show distance) commandTemplate 
       cmd  = replace "%s" (poiDescription poi) cmd'
   debug $ "Launching command " ++ cmd
   result <- try $ system cmd :: IO (Either SomeException ExitCode)
   case result of
      Left err -> warning $ "Couldn't launch command. It failed with " ++ show err
      Right ex -> debug $ "Command exited with exit code " ++ show ex
   return ()

data DBType = CSV String | SQLITE3 String (String, String, String, String, Maybe String) 
              deriving (Show)

-- | Splits a list
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p [] = [[]]
splitWhen p l  = filter ((/=0) . length) . uncurry (:) . fmap (splitWhen p . drop 1) . break p $ l

-- | Parses a database description into a DBType
parseDBDescr :: String -> DBType
parseDBDescr fn 
      | length params == 1 = CSV fn
      | length params == 5 = SQLITE3 (params !! 0) (params !! 1, params !! 2, params !! 3, params !! 4, Nothing)
      | length params == 6 = SQLITE3 (params !! 0) (params !! 1, params !! 2, params !! 3, params !! 4, Just $ params !! 5)
      | otherwise = error "Cannot parse database description."
      where params = splitWhen (== ':') fn

main :: IO ()
main = do
   myStreamHandler <- streamHandler stderr DEBUG
   let handler = setFormatter myStreamHandler $ simpleLogFormatter "[$time : $loggername : $prio] $msg"
   updateGlobalLogger rootLoggerName (setHandlers [handler])
   args' <- getArgs
   args <- if (length args' > 0) && args' !! 0 == "debug"
              then do updateGlobalLogger myName (setLevel DEBUG)
                      return $ tail args'
              else return args'
   when (length args < 4) $ do
      hPutStrLn stderr "Usage: POIWarner [debug] gpsd_host gpsd_port warn_command database1 ..."
      hPutStrLn stderr "When you approach a POI, warn_command is passed to the shell."
      hPutStrLn stderr "Any occurrence of %d is replaced by the distance to the POI"
      hPutStrLn stderr "and %s is replaced by the description of the POI."
      hPutStrLn stderr "Example: POIWarner 127.0.0.1 2947 'echo POI %s in %d' poi1.csv poi2.csv"
      hPutStrLn stderr "Instead of the .csv file you can also use a sqlite3 database. In this"
      hPutStrLn stderr "supply 'db.sqlite3:tablename:latitudeField:longitudeField:poiTypeField'"
      hPutStrLn stderr "or 'db.sqlite3:tbl:lat:lon:poiTypeField:additionalWhereClause'"
      hPutStrLn stderr "as database file name."
      exitFailure
   gps <- initGps (args !! 0) (read $ args !! 1)
   waitForFix gps
   tv <- newTVarIO []
   forM_ (drop 3 args) $ \filename -> do
      forkIO $ databaseUpdateThread (parseDBDescr filename) gps radiusHotPOIs tv
   poiWarnerThread tv gps radiusHotPOIs (args !! 2)
