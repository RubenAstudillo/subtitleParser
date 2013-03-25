module Main where

import Test.HUnit
import Test.HUnit.Text
import Data.Text.IO as TI 
import Data.Text (pack)
import Data.Attoparsec.Text
import Text.Subtitles.SRT

main :: IO ()
main = runTestTT (TestList [test srtAssert]) >> return ()

srtAssert :: Assertion
srtAssert = do
  srtContents <- TI.readFile "./test/example.srt"
  case parseOnly parseSRT srtContents of
    Left _ -> assertFailure "Error while parsing the example .srt"
    Right r -> assertEqual "Parser didn't produce the expected value" expectedValue r

{- I wanted the parser to produce this output, when it accomplish it, it will be
 - complete -}
expectedValue :: Subtitles
expectedValue = [ Line 1 (Range (Time 0 2 26 407) (Time 0 2 31 356)) (Just (R 100 100 100 100)) (pack "<font color=\"#00ff00\">Detta handlar om min storebrors</font> \n<b><i><u>kriminella beteende och foersvinnade.</u></i></b>"),
  Line 2 (Range (Time 0 2 31 567) (Time 0 2 37 164)) Nothing (pack "Vi talar inte laengre om Wade. Det aer \nsom om han aldrig hade existerat.")]

