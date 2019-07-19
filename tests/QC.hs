{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main (main) where

import qualified QC.Buffer as Buffer
import qualified QC.ByteString as ByteString
import qualified QC.Combinator as Combinator
import qualified QC.Simple as Simple
import qualified QC.Text as Text

main :: IO ()
main = do
  putStrLn "ByteString" >> ByteString.tests
  putStrLn "Buffer" >> Buffer.tests
  putStrLn "Combinator" >> Combinator.tests
  putStrLn "Simple" >> Simple.tests
  putStrLn "Text" >> Text.tests
