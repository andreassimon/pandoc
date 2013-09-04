import Text.Regex.Posix
import Text.Pandoc
import Text.Pandoc.Definition

readDoc :: String -> Pandoc
readDoc = readMarkdown def 

readSampleDoc :: Pandoc
readSampleDoc = readDoc (
              "# Heading 1\n" ++
              "**Wer das liest** ist doof\n\n" ++
              "''Remfert'' und ''Teubner'' schreiben nur Bullshit."
            )

writeDoc :: Pandoc -> String
writeDoc = writeLaTeX def

para :: Pandoc -> [Block]
para (Pandoc _ p) = p

transform :: Pandoc -> Pandoc
transform (Pandoc m bs) = (Pandoc m $ map transformBlock bs)

transformBlock :: Block -> Block
transformBlock (Para inlines) = (Para $ map transformInline inlines)
transformBlock b = b

transformInline :: Inline -> Inline
-- transformInline (Str ['\'','\'','R','e','m','f','e','r','t','\'','\'']) = SmallCaps [Str "Remfert"]
transformInline (Str s)
  | s =~ "''(\\w+)''" = SmallCaps [Str (stripQuotes s) ]
  | otherwise          = Str s
transformInline i = i

stripQuotes s = head $ tail $ head ((s =~ "''(\\w+)''") :: [[String]])

convert s = writeDoc $ transform $ readDoc s

main :: IO ()
main = do
  putStr $ convert "# Heading 1"
  putStr "\n"
  putStr $ convert "**Wer das liest** ist doof"
  putStr "\n"
  putStr $ show $ para $ transform $ readDoc "**Wer das liest** ist doof"
  putStr "\n"
  putStr "\n"
  putStr $ convert "''Teubner und Remfert''"
  putStr $ show $ para $ transform $ readDoc  "''Teubner und Remfert''"
  putStr "\n"
  putStr $ convert "''Remfert''"
  putStr "\n"
  putStr $ writeDoc $ transform $ readSampleDoc
  -- interact (writeDoc . readDoc)
