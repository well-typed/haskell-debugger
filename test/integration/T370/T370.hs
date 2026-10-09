module T370 where
import Control.Concurrent
import Control.Concurrent.MVar

main :: IO ()
main = do
  v0 <- newEmptyMVar
  v1 <- newEmptyMVar
  v2 <- newEmptyMVar
  forkIO $ f 0 >>= putMVar v0
  forkIO $ f 1 >>= putMVar v1
  forkIO $ f 2 >>= putMVar v2
  takeMVar v0
  takeMVar v1
  takeMVar v2

f :: Int -> IO ()
f n | n < 2 = g n
    | otherwise = h n

g :: Int -> IO ()
g x = do
  print x
  print (x*2)
  pure ()

h :: Int -> IO ()
h y = do
  print y
  print (y*2)
  pure ()

