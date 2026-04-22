module MyLib (thenDo) where

thenDo :: (String -> IO String) -> String -> IO ()
thenDo a b = do
  print =<< a ("test" ++ b)
{-# OPAQUE thenDo #-}
