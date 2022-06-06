{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main
  ( main     -- payload
  , v, test  -- for testing
  ) where

import GHC.Exts (fromList)
import GHC.Generics

-- import Control.Arrow ((&&&))
import Data.Aeson hiding ((.=))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (sort, sortOn)
import Data.List.Extra (groupOn)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)

-- For plotting
import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

------------------------------------------------------------------------
-- * Configuration
------------------------------------------------------------------------

-- | Date when migration to @github@ was complete.
--   We only consider issues reported after the migration.
--   For the others, the @created_at@ date is not correct.

migrationDate :: UTCTime
migrationDate = timeFromString "2015-08-08T23:59:59+00:00"

migrationDay :: String
migrationDay = "2015-08-08"

migrationYear :: String
migrationYear = "2015"

-- | Filename prefix for generated charts.

pngPrefix :: String
pngPrefix = "agda-issues-"

------------------------------------------------------------------------
-- * Types
------------------------------------------------------------------------

-- | Bug database as obtained from github.

data AgdaIssues = AgdaIssues
  { agda :: Map Int Issue  -- ^ Maps issue number to 'Issue'.
      -- Not IntMap, because this has a different encoding/decoding
  } deriving (Generic, Show)

-- | Issue information as obtained from github.

data Issue = Issue
  { assignee        :: String
  , closed_at       :: Maybe UTCTime
  , created_at      :: UTCTime
  , is_pull_request :: Bool
  -- , labels          :: [String]
  } deriving (Generic, Show)

instance ToJSON   Issue where
instance FromJSON Issue where

instance ToJSON   AgdaIssues where
instance FromJSON AgdaIssues where

-- * Main
------------------------------------------------------------------------

-- | Main program spaghetti code.

main :: IO ()
main = do

  -- Parse JSON file.
  ------------------------------------------------------------------------

  issues :: AgdaIssues <- do
    t <- parseFile "agdaissues.json"
    case fromJSON t of
        Error   s -> error s
        Success x -> return x

  -- List of issues
  let is :: [(Int,Issue)]
      is = Map.toList $ agda issues

  -- -- Latest issue
  -- let latestIssue :: Issue
  --     latestIssue = maybe undefined snd $ Map.lookupMax issues

  -- Chart for lifetime of open issues.
  ------------------------------------------------------------------------

  now <- getCurrentTime

  -- Today, e.g. 2022-06-06
  let nowDay :: String
      nowDay = iso8601Show $ utctDay now
      nowYear :: String
      nowYear = take 4 $ nowDay  -- TODO in year 9999: fixme

  -- UNUSED:
  -- let openissues = filter ((not . is_pull_request &&? isNothing . closed_at) . snd) is

  let ageInDays = round . (/ nominalDay) . diffUTCTime now
      maxAge    = ageInDays migrationDate

      -- We consider only issues opened after the migration to github
      recentissues = filter ((not . is_pull_request &&? (migrationDate <) . created_at) . snd) is
      recentopenissues = filter (isNothing . closed_at . snd) recentissues

      -- Recent open issues sorted by age in days
      ageopenissues :: [Int]
      ageopenissues = sort $ map (ageInDays . created_at . snd) recentopenissues

      -- Number of issues older than x days
      ageByNo :: [(Int,Int)]
      ageByNo = keepLast $ zip ageopenissues [1..]

  -- putStrLn $ "Open issues: " ++ show (length openissues)
  -- forM_ openissues $ \ (no, i) -> print no
  -- mapM_ print ageopenissues
  -- mapM_ print ageByNo

  let dateRange = unwords [ migrationDay, "to", nowDay ]
  let pngSuffix = concat [ "-", migrationYear, "-", nowYear, ".png" ]

  toFile def (concat [pngPrefix, "age-open", pngSuffix]) $ do
    layout_title .= unwords [ "Open issue age (cumulative),", dateRange ]
    plotBarsRedGreen $ bars ["# with that age in days"] $
      map (\ (age, n) -> (age, [n])) ageByNo

  -- Chart for lifetime of closed issues.
  ------------------------------------------------------------------------

  -- Issues reported after migration and closed
  let recentclosedissues =  filter (isJust . closed_at . snd) recentissues

  -- Recent closed issues sorted by age in days
  let ageclosedissues :: [Int]
      ageclosedissues = sort $
        map (round . (/ nominalDay)
                   . (\ i -> diffUTCTime (fromJust $ closed_at i) (created_at i))
                   . snd)
            recentclosedissues

      -- Number of issues that lived longer than x days
      closedAgeByNo :: [(Int,Int)]
      -- unoptimized, contains several y-values for some x-values (smaller y-values will be overwritten in chart)
      -- after @keepLast@ optimized, only keep largest (= last) y-value per x-value
      closedAgeByNo = keepLast $ zip ageclosedissues [1..]

  -- Linear scale
  toFile def (concat [pngPrefix, "age-closed", pngSuffix]) $ do
    layout_title .= unwords [ "Closed issue age (cumulative),", dateRange ]
    -- point_border_width .= 0.0 -- NOT POSSIBLE
    -- setColors [opaque limegreen]
    plotBarGreen $ bars ["# with that age in days"] $
      map (\ (age, n) -> (age, [n])) closedAgeByNo

  -- Logarithmic scale
  toFile def (concat [pngPrefix, "age-closed-log", pngSuffix]) $ do
    layout_title .= unwords [ "Closed issue age logarithmically (cumulative),", dateRange ]
    plotBarGreen $ bars ["# with exp of that age"] $
      map (\ (age, n) -> (LogValue (fromIntegral age), [n])) closedAgeByNo

  -- Plot open issues vs. total issues over time.
  ------------------------------------------------------------------------

  -- Sort issues by birth age.
  let issuesByAge = sortOn created_at $ map snd recentissues
      initial :: (Int, Int, Int)
      initial = (0, 0, 0)

      -- UNUSED:
      -- stepOT (_age, open, total) i =
      --   ( ageInDays $ created_at i
      --   , if isNothing (closed_at i) then open + 1 else open
      --   , total + 1
      --   )
      -- openVsTotal = scanl stepOT initial issuesByAge

      stepOC (_age, open, closed) i =
        ( ageInDays $ created_at i
        , if isNothing (closed_at i) then open + 1 else open
        , if isJust (closed_at i) then closed + 1 else closed
        )
      openVsClosed = scanl stepOC initial issuesByAge

  toFile def (concat [pngPrefix, "open-vs-closed", pngSuffix]) $ do
    layout_title .= unwords [ "Open vs closed issues,", dateRange ]
    -- point_border_width .= 0.0 -- NOT POSSIBLE
    -- setColors [opaque limegreen]
    plotBarsRedGreen $ bars ["open issues", "closed issues"] $
      map (\ (t, o, c) -> (maxAge - t, [o,c])) openVsClosed


  -- Print some basic statistics.
  ------------------------------------------------------------------------

  let ni = length recentissues
      ci = length recentclosedissues
      oi = length recentopenissues
  putStrLn $ concat [ "Only counting issues after migration to github on ", migrationDay, "." ]
  putStrLn $ "Total  issues: " ++ show ni
  putStrLn $ "Closed issues: " ++ show ci
                ++ " (" ++ show (100 * ci `div` ni) ++ "%)"
  putStrLn $ "Open   issues: " ++ show oi
                ++ " (" ++ show (100 * oi `div` ni) ++ "%)"

  where
    plotBarsRedGreen b = plot $ barsRedGreen <$> b
    plotBarGreen     b = plot $ barGreen     <$> b

-- ** Plotting helpers

solidGreen, solidRed :: FillStyle
solidGreen = FillStyleSolid $ opaque limegreen
solidRed   = FillStyleSolid $ opaque crimson

barGreen :: PlotBars a Int -> Plot a Int
barGreen =
  plotBars
  . set plot_bars_style BarsStacked
  . set plot_bars_item_styles (map (,Nothing) [ solidGreen ])

barsRedGreen :: PlotBars a Int -> Plot a Int
barsRedGreen =
  plotBars
  . set plot_bars_style BarsStacked
  . set plot_bars_item_styles (map (,Nothing) [ solidRed, solidGreen ])

-- ** Data helpers

-- | Given a sorted list of (x,y) pairs, keep for each x-value only the largest (= last) y-value.

keepLast :: Eq y => [(x,y)] -> [(x,y)]
keepLast = map last . groupOn snd

------------------------------------------------------------------------
-- * Auxiliary functions
------------------------------------------------------------------------

-- | Pointwise conjunction.

(&&?) :: (Issue -> Bool) -> (Issue -> Bool) -> Issue -> Bool
p &&? q = \ x -> p x && q x

infixl 2 &&?

-- | Time parsing from github/python formatted zoned-time values.

timeFromString :: String -> UTCTime
timeFromString =
  maybe (error "not a valid time value") zonedTimeToUTC . iso8601ParseM

-- | Read a JSON 'Value' from a file.

parseFile :: FilePath -> IO Value
parseFile f = do
  s <- BS.readFile f
  return $ fromMaybe (error "json parse failed") $ decode s

------------------------------------------------------------------------
-- * Testing
------------------------------------------------------------------------

-- | A sample issue listing.

v :: AgdaIssues
v = AgdaIssues $ Map.fromList
  [ (1, Issue
       { assignee        = "None"
       , closed_at       = Just $ timeFromString "2015-08-08T17:51:50+00:00"
       , created_at      = timeFromString "2015-08-08T17:51:49+00:00"
       , is_pull_request = False
       -- , labels          = ["auto-migrated", "bug", "priority-low"]
       })
  ]

-- | An experiment to produce JSON from 'Value' representation.

test :: ByteString
test = encode
  . Object . fromList . (:[]) . ( "agda",)
  . Object . fromList $
  [ ("1",) . Object . fromList $
    [ ("assignee",)        $ String "None"
    , ("closed_at",)       $ String "2015-08-08T17:51:50+00:00"
    , ("created_at",)      $ String "2015-08-08T17:51:49+00:00"
    , ("is_pull_request",) $ Bool False
    -- , ("labels",) . Array . fromList $ [ "auto-migrated" , "bug", "priority-low" ]
    ]
  , ("2",) . Object . fromList $
    [ ("assignee",)        $ String "None"
    , ("closed_at",)       $ Null
    , ("created_at",)      $ String "2015-08-08T17:51:49+00:00"
    , ("is_pull_request",) $ Bool False
    -- , ("labels",) . Array . fromList $ [ "auto-migrated" , "bug", "priority-low" ]
    ]
  ]
