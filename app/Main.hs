module Main where

import qualified IMP.Language as IMP

main :: IO ()
main = let x = IMP.Add (IMP.LI 10) (IMP.LI 20) in
           let r = IMP.interpret x in
               do
                   putStrLn $ show (r :: Int)
