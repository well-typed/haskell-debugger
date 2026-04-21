f () = do
  let xs = [3,2,1]
  pure ()
  print xs

main = do
  let doit = const (pure () :: IO ())
  let xs = [4,5,6]
  doit ()
  f ()
  print xs
  putStrLn "done"
