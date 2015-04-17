module Hutton.HTTP where

import Data.ByteString.Lazy.Char8 (unpack)
import Network.HTTP.Conduit (simpleHttp)
import Text.Regex (matchRegex, mkRegex)

getParameters :: IO (Maybe (String, String))
getParameters = do
    response <- simpleHttp "https://www.reddit.com/r/thebutton"
    let needle = mkRegex "\\?h=([0-9a-f]+)&e=([0-9]+)"
        haystack = unpack response
        matches = matchRegex needle haystack
    case matches of
        Just [h, e] -> return (Just (h, e))
        _ -> return Nothing
