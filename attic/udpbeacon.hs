module Main where
import Network.Socket
import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Network.Multicast

port = "5670"
mCast = "225.25.25.25"

main = do
    (addr:_) <- getAddrInfo Nothing (Just mCast) (Just port)
    --a <- async $ beaconRecv
    --b <- async $ beacon addr
    res <- concurrently beaconRecv (beacon addr)
    return ()

beaconRecv = do
    sock <- multicastReceiver mCast 5670
    forever $ do
        (msg, _, addr) <- recvFrom sock 1024
        print (msg, addr)

beacon addr = do
    withSocketsDo $ do
      bracket (getSocket addr) sClose (talk (addrAddress addr))

    putStrLn "/"
  where
    getSocket addr = do
      s <- socket (addrFamily addr) Datagram defaultProtocol
      mapM_ (\x -> setSocketOption s x 1) [Broadcast, ReuseAddr, ReusePort]
      bind s (addrAddress addr)
      return s
    talk addr s = forever $ do
      putStrLn "send"
      sendTo s "ZRE" addr
      threadDelay 1000000


--  transmit = struct.pack('cccb16sH', b'Z', b'R', b'E',
--                    BEACON_VERSION, self.identity.bytes,
--                    socket.htons(self.port))
