{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import GHC.Exts (fromList)
import GHC.Generics

import Data.Aeson hiding ((.=))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (sort, sortOn)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM)

-- For plotting
import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

-- | Bug database as obtained from github.

data AgdaIssues = AgdaIssues
  { agda :: Map Int Issue  -- ^ Maps issue number to 'Issue'.
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

-- | Time parsing from github/python formatted zoned-time values.

timeFromString :: String -> UTCTime
timeFromString =
  maybe (error "not a valid time value") zonedTimeToUTC . iso8601ParseM

-- | Pointwise conjunction.

(&&?) :: (Issue -> Bool) -> (Issue -> Bool) -> Issue -> Bool
p &&? q = \ x -> p x && q x

infixl 2 &&?

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

-- | Date when migration to @github@ was complete.
--   We only consider issues reported after the migration.
--   For the others, the @created_at@ date is not correct.

migrationDate :: UTCTime
migrationDate = timeFromString "2015-08-08T23:59:59+00:00"

-- | Read a JSON 'Value' from a file.

parseFile :: FilePath -> IO Value
parseFile f = do
  s <- BS.readFile f
  return $ fromMaybe (error "json parse failed") $ decode s

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

  -- Chart for livetime of open issues.
  ------------------------------------------------------------------------

  now <- getCurrentTime

  let openissues = filter ((not . is_pull_request &&? isNothing . closed_at) . snd) is

      ageInDays = round . (/ nominalDay) . diffUTCTime now
      maxAge    = ageInDays migrationDate

      -- We consider only issues opened after the migration to github
      recentissues = filter ((not . is_pull_request &&? (migrationDate <) . created_at) . snd) is
      recentopenissues = filter (isNothing . closed_at . snd) recentissues

      -- Recent open issues sorted by age in days
      ageopenissues :: [Int]
      ageopenissues = sort $ map (ageInDays . created_at . snd) recentopenissues

      -- Number of issues older than x days
      ageByNo :: [(Int,Int)]
      ageByNo = zip ageopenissues [1..]

  -- putStrLn $ "Open issues: " ++ show (length openissues)
  -- forM_ openissues $ \ (no, i) -> print no
  -- mapM_ print ageopenissues
  -- mapM_ print ageByNo

  toFile def "agda-issues-age-open-2015-2022.png" $ do
    layout_title .= "Open issue age (cumulative), 2015-08-08 to 2022-06-06"
    plot $ plotBars
         . set plot_bars_style BarsStacked
         . set plot_bars_item_styles
             [ (FillStyleSolid $ opaque crimson  , Nothing)
             , (FillStyleSolid $ opaque limegreen, Nothing)
             ]
           <$> bars ["days"] (map (\ (age, n) -> (age, [n])) ageByNo)

  -- Chart for lifetime of closed issues.
  ------------------------------------------------------------------------

  -- Issues reported after migration and closed
  let recentclosedissues =  filter (isJust . closed_at . snd) recentissues

      -- Recent closed issues sorted by age in days
      ageclosedissues :: [Int]
      ageclosedissues = sort $ map (round . (/ nominalDay) . (\ i -> diffUTCTime (fromJust . closed_at $ i) (created_at i)) . snd) recentclosedissues

      -- Number of issues that lived longer than x days
      closedAgeByNo :: [(Int,Int)]
      closedAgeByNo = zip ageclosedissues [1..]

  toFile def "agda-issues-age-closed-2015-2022.png" $ do
    layout_title .= "Closed issue age (cumulative), 2015-08-08 to 2022-06-06"
    -- point_border_width .= 0.0 -- NOT POSSIBLE
    -- setColors [opaque limegreen]
    plot $ plotBars
         . set plot_bars_style BarsStacked
         . set plot_bars_item_styles
             [ (FillStyleSolid $ opaque limegreen, Nothing)
             ]
           <$> bars ["days"] (map (\ (age, n) -> (age, [n])) closedAgeByNo)

  toFile def "agda-issues-age-closed-log-2015-2022.png" $ do
    layout_title .= "Closed issue age logarithmically (cumulative), 2015-08-08 to 2022-06-06"
    plot $ plotBars
         . set plot_bars_style BarsStacked
         . set plot_bars_item_styles
             [ (FillStyleSolid $ opaque limegreen, Nothing)
             ]
           <$> bars ["days"] (map (\ (age, n) -> (LogValue (fromIntegral age), [n])) closedAgeByNo)

  -- Plot open issues vs. total issues over time.
  ------------------------------------------------------------------------

  -- Sort issues by birth age.
  let issuesByAge = sortOn created_at $ map snd recentissues
      initial :: (Int, Int, Int)
      initial = (0, 0, 0)
      stepOT (_age, open, total) i =
        ( ageInDays $ created_at i
        , if isNothing (closed_at i) then open + 1 else open
        , total + 1
        )
      openVsTotal = scanl stepOT initial issuesByAge
      stepOC (_age, open, closed) i =
        ( ageInDays $ created_at i
        , if isNothing (closed_at i) then open + 1 else open
        , if isJust (closed_at i) then closed + 1 else closed
        )
      openVsClosed = scanl stepOC initial issuesByAge

  toFile def "agda-issues-open-vs-closed-2015-2022.png" $ do
    layout_title .= "Open vs closed issues 2015-08-08 to 2022-06-06"
    -- point_border_width .= 0.0 -- NOT POSSIBLE
    -- setColors [opaque limegreen]
    plot $ plotBars
         . set plot_bars_style BarsStacked
         . set plot_bars_item_styles
             [ (FillStyleSolid $ opaque crimson  , Nothing)
             , (FillStyleSolid $ opaque limegreen, Nothing)
             ]
         <$> bars ["open issues", "closed issues"] (map (\ (t, o, c) -> (maxAge - t, [o,c])) openVsClosed)


  -- Print some basic statistics.
  ------------------------------------------------------------------------

  let ni = (length recentissues)
      ci = (length recentclosedissues)
      oi = (length recentopenissues)
  putStrLn $ "Only counting issues after migration to github on 2015-08-08."
  putStrLn $ "Total  issues: " ++ show ni
  putStrLn $ "Closed issues: " ++ show ci
                ++ " (" ++ show (100 * ci `div` ni) ++ "%)"
  putStrLn $ "Open   issues: " ++ show oi
                ++ " (" ++ show (100 * oi `div` ni) ++ "%)"
