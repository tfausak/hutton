module Hutton.WebSockets where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (toStrict)
import Flow
import Network.Connection (Connection, ConnectionParams (..), TLSSettings (..),
    connectionGetChunk, connectionPut, connectTo, initConnectionContext)
import Network.Socket (PortNumber (..))
import Network.WebSockets (ClientApp, ConnectionOptions, Headers,
    defaultConnectionOptions, runClientWithStream)
import Network.WebSockets.Stream (makeStream)

runSecureClient :: String -> PortNumber -> String -> ClientApp a -> IO a
runSecureClient host port path app = do
    context <- initConnectionContext
    connection <- connectTo context (connectionParams host port)
    stream <- makeStream (reader connection) (writer connection)
    runClientWithStream stream host path connectionOptions headers app

connectionParams :: String -> PortNumber -> ConnectionParams
connectionParams host port = ConnectionParams
    { connectionHostname = host
    , connectionPort = port
    , connectionUseSecure = Just tlsSettings
    , connectionUseSocks = Nothing
    }

tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple
    { settingDisableCertificateValidation = False
    , settingDisableSession = False
    , settingUseServerName = False
    }

reader :: Connection -> IO (Maybe BS.ByteString)
reader connection = fmap Just (connectionGetChunk connection)

writer :: Connection -> Maybe BL.ByteString -> IO ()
writer connection = maybe (return ()) (toStrict .> connectionPut connection)

connectionOptions :: ConnectionOptions
connectionOptions = defaultConnectionOptions

headers :: Headers
headers = []
