newtype D a = D {unD :: D a -> a}

main = do
  let doit = const (pure () :: IO ())
  let y = D (const $ 'y')
  let x :: (a ~ IO ()) => a
      x = print ()
  putStrLn "helio"
  doit ()
  print $ unD y y
  x
