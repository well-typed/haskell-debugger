module Main where
import GHC.Debugger.View.Class

data X = X String
    deriving Show

data Y = Y String
    deriving Show

instance DebugView X where
    debugValue _ = VarValue "SDJFLSKDJFLKSDJFLSJDKFL" True
    debugFields (X s) = VarFields
        [ ("field1", (VarFieldValue s))
        , ("myfield2", (VarFieldValue (length s)))
        , ("field4", (VarFieldValue (2345 :: Int)))
        , ("field5", (VarFieldValue (2345 :: Double)))
        -- important! test no-debug-view type inside of debug-view instance. this used to crash:
        , ("field3", (VarFieldValue (Y (show (length "inner Y")))))
        ]

main :: IO ()
main = f (X "A33")

f :: Show a => a -> IO ()
f action = do
    print action
