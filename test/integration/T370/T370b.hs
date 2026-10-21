module T370b where

main :: IO ()
main = do
  x <- pure "foo"
  pure ()
  print x
  main
