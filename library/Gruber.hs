module Gruber (module Gruber) where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim (many, parse, try)
import Control.Applicative ((<$>),(<|>), (<*))
import Control.Monad (void)

data Component =
                Heading1 String |
                Heading2 String |
                Heading3 String deriving (Show, Eq, Ord)

type Document = [Component]

stringTillNewLine :: Parser String
stringTillNewLine = do
        c <- many1 (noneOf "\n")
        newline >> return c

emptyLine :: Parser ()
emptyLine = void newline

headingParser :: Parser Component
headingParser = do
        try (Heading1 <$> (string "# " >> stringTillNewLine))
    <|> try (Heading2 <$> (string "## " >> stringTillNewLine))
    <|> try (Heading3 <$> (string "### " >> stringTillNewLine))

componentParser :: Parser Component
componentParser =
        headingParser <* emptyLine

documentParser :: Parser Document
documentParser = many componentParser <* eof

main :: IO ()
main = do 
        c <- getContents
        case parse documentParser "(stdin)" c of
            Left e -> do
                        putStrLn "Error parsing input: "
                        print e
            Right r -> mapM_ print r

