module Main where
import GHC.Debugger.View.Class

data X = X String
    deriving Show

instance DebugView X where
    debugValue _ = simpleValue "T301-X" True
    debugFields (X s) = pure $ VarFields
        [ ("thunkInt", VarFieldValue intThunk)
        ]
      where
        intThunk :: Int
        intThunk = sum [1 .. length s + 99]

main :: IO ()
main = f (X "A")

f :: Show a => a -> IO ()
f action = do
    print action
