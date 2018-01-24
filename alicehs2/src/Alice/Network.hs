module Alice.Network where

--import           Alice.Utils
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import qualified Data.ByteString.Char8         as C
import           Network.Socket                hiding (recv, send)
import           Network.Socket.ByteString

start :: HostName -> Int -> IO Socket
start hostname port = do
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just $ show port)
  let serveraddr = head addrinfos
  socket (addrFamily serveraddr) Stream defaultProtocol

getNetworkData :: Socket -> IO C.ByteString
getNetworkData sock = do
  send sock $ C.pack "store"
  recv sock 1024
