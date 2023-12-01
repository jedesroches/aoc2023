module Lib
    ( runOnLines
    ) where

runOnLines :: (Show r) => ([String] -> r) -> IO ()
runOnLines f = getContents >>= print . f . lines
