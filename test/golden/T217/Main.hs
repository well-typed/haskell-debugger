newtype D a = D {unD :: D a -> a}
newtype B = B (Either String Bool)
  deriving Show
{-# OPAQUE f #-}
f :: B -> Either B B
f b = Left b

main = do
  let doit = const (pure () :: IO ())
  let y = D (const $ 'y')
  let x :: (a ~ IO ()) => a
      x = print ()
  -- to preserve newtype constructor `B` in reconstructed Term we must not force `ix`
  -- until after the breakpoint.
  ix <- pure $ B (Left "A")
  putStrLn "helio"
  doit ()
  print $ unD y y
  x
  print (f ix)
