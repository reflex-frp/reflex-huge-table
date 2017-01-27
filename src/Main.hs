{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.String.Class
import Reflex
import Reflex.Dom
import System.Exit

main :: IO ()
main =
  mainWidget $ do
    clicked <- delay 0.1 =<< getPostBuild
    done <- delay 10 clicked
    performEvent_ $ liftIO exitSuccess <$ done
    void . widgetHold blank $ ffor clicked $ \_ -> do
      let colNames = ["a","b","c","d"]
      elClass "table" "table" $ do
        el "thead" . el "tr" $ forM_ colNames $ \n ->
            el "th" (text n)
        el "tbody" . forM_ table $ \Entry{..} -> do
          let cells = fromString <$> [a, b, show c, show d]
          el "tr" . forM_ cells $ \cell -> do
                el "td" (text cell)

data Entry = Entry
  { a :: String
  , b :: String
  , c :: Double
  , d :: Int
  }

table :: [Entry]
table = replicate 1000 Entry {a = "Some", b = "Text here", c = 1.23, d = 424242}
