{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Lens
import           Control.Lens.At       (at)
import           Data.Aeson            (FromJSON, Object (..), Value (..),
                                        (.!=), (.:), (.:?))
import           Data.Aeson.Types      (parseEither)
import           Data.Int              (Int64)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (maybeToList)
import           Data.Monoid           (mconcat)
import           Data.String           (fromString)
import           Data.Text             (Text, pack, unpack)
import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Database.InfluxDB     (Field (..), InfluxException (..), Key,
                                        Line (..), LineField, WriteParams, host,
                                        server, write, writeParams)
import           Options.Applicative   (Parser, auto, execParser, fullDesc,
                                        help, helper, info, long, option,
                                        progDesc, showDefault, strOption, value,
                                        (<**>))
import           Web.Scotty            (json, jsonData, liftAndCatchIO, post,
                                        raise, scotty)

data Options = Options {
  optInfluxDBHost :: Text
  , optInfluxDB   :: String
  , optListenPort :: Int
  }

options :: Parser Options
options = Options
  <$> strOption (long "dbhost" <> showDefault <> value "localhost" <> help "influxdb host")
  <*> strOption (long "dbname" <> showDefault <> value "owntracks" <> help "influxdb database")
  <*> option auto (long "port" <> showDefault <> value 3000 <> help "listen port")

commonFields :: Object -> Either String [(Key, LineField)]
commonFields v = flip parseEither v $ \obj -> do
  acc <- obj .: "acc"
  lat <- obj .: "lat"
  lon <- obj .: "lon"
  pure [("acc", FieldFloat acc),
        ("lat", FieldFloat lat),
        ("lon", FieldFloat lon)]

identify :: Object -> Either String (String, UTCTime)
identify v = flip parseEither v $ \obj -> do
  tst <- obj .: "tst"
  tid <- obj .: "tid"
  let ts = posixSecondsToUTCTime . fromIntegral $ (tst::Int64)
  pure (tid, ts)

j2ml :: Maybe Text -> Object -> Either String (Line UTCTime)
j2ml (Just "location") v = do
  common <- commonFields v
  (tid, ts) <- identify v
  flip parseEither v $ \obj -> do
    alt <- obj .: "alt"
    batt <- obj .: "batt"
    conn <- obj .: "conn"
    vel <- obj .: "vel"
    regs <- obj .:? "inregions" .!= []
    let tags = Map.fromList ([("tid", fromString tid)] <> (take 1 . map (\x -> ("loc", fromString x))) regs)
    pure $ Line "location" tags (Map.fromList (common <> [
                                                  ("alt", FieldFloat alt),
                                                  ("batt", FieldFloat batt),
                                                  ("conn", FieldString conn),
                                                  ("vel", FieldFloat vel)
                                                  ])) (Just ts)

j2ml (Just "transition") v = do
  common <- commonFields v
  (tid, ts) <- identify v
  flip parseEither v $ \obj -> do
    wtst <- obj .: "wtst"
    ev <- obj .: "event"
    desc <- obj .: "desc"
    t <- obj .: "t"
    let tags = Map.fromList [("tid", fromString tid), ("loc", fromString desc)]
    pure $ Line "transition" tags (Map.fromList (common <> [
                                                    ("wtst", FieldInt wtst),
                                                    ("ev", FieldString ev),
                                                    ("t", FieldString t)
                                                    ])) (Just ts)


j2ml l v = Left $  mconcat $ ["not understood: ", show l, " ", show v]

run :: Options -> IO ()
run Options{..} = do
  let wp = writeParams (fromString optInfluxDB) & server.host .~ optInfluxDBHost

  scotty optListenPort $
    post "/" $ do
      t <- jsonData
      case j2ml (typ t) t of
        Left err -> raise . fromString $ err
        Right l  -> liftAndCatchIO $ write wp l
      json Null

        where
          typ :: Object -> Maybe Text
          typ j = (\(String x) -> x) <$> j ^. at "_type"

main :: IO ()
main = run =<< execParser opts

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Influx my own tracks")
