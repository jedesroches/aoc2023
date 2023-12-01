module Adventlib
    ( runOnLines
    ) where


-- Useful stuff
runOnLines :: (Show r) => ([String] -> r) -> IO ()
runOnLines f = getContents >>= print . f . lines
