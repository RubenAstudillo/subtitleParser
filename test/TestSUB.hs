module Main where

import Test.HUnit
import Test.HUnit.Text
import Text.Subtitles.SUB
import Data.Text (pack)
import Data.Text.IO as TI (readFile)
import Data.Attoparsec.Text (parseOnly)

main :: IO ()
main = runTestTT (test assertSUB) >> return ()

expectedSUB :: Subtitles
expectedSUB = [Line 13187 13215 (Just Italic) (pack "Áno!"),
  Line 13430 13456 Nothing (pack "Áno!"),
  Line 13935 14020 (Just (C (pack "$0000ff"))) (pack "Nezdie¾am vašu rados..."),
  Line 14034 14147 Nothing (pack "- Dobrá práca, agentka Marcusová.|- Ïakujem.")]

assertSUB :: Assertion
assertSUB = do
  example <- TI.readFile "./test/example.sub"
  case parseOnly parseSUB example of
    Left _ -> assertFailure "parseSUB failed on example"
    Right r -> assertEqual "parseSUB result didn't match expected" expectedSUB r
