module CSV.CSV (
  parseCSV
  , printCSVParserError) where

  import Text.ParserCombinators.Parsec

  csvFile = endBy line eol
  line = sepBy cell (char ',')
  cell = quotedCell <|> many (noneOf ",\n\r")

  quotedCell = 
      do char '"'
         content <- many quotedChar
         char '"' <?> "quote at end of cell"
         return content

  quotedChar =
          noneOf "\""
      <|> try (string "\"\"" >> return '"')

  eol =   try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

  parseCSV :: String -> Either ParseError [[String]]
  parseCSV input = parse csvFile "Tweeter Src" input

  printCSVParserError :: ParseError -> IO ()
  printCSVParserError e = do
    putStrLn "Ah crap! Unable to parse CSV file: "
    print e
