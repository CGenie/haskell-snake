{-# LANGUAGE ForeignFunctionInterface #-}
module MainWrapper where
import Main (main)
foreign export ccall "haskell_main" main :: IO ()
