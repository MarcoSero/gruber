module Gruber (module Gruber) where

import Control.Applicative ((<$>), (<*))
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String

data Inline =
                Emphasis Inline
            |   StrongEmphasis Inline
            |   Code String
            |   Strikethrough Inline
            |   NotFormatted String
            deriving (Eq, Show, Ord)

type Paragraph = [Inline]

data Component  =
                    Heading1 Paragraph
                |   Heading2 Paragraph
                |   Heading3 Paragraph
                |   Heading4 Paragraph
                |   Heading5 Paragraph
                |   Heading6 Paragraph
                |   HorizontalRule
                deriving (Show, Eq, Ord)

type Document = [Component]

inline :: Parsec String () Inline
inline = recurse <|> terminal
    where   terminal = do
                    let reservedChars = "\n*_`"
                    try (NotFormatted <$> many1 (noneOf reservedChars))
                <|> try (Code <$> between (string "`") (string "`") (many (noneOf "\n")))
            recurse = do
                    try (Emphasis <$> between (string "_") (string "_") inline)
                <|> try (Emphasis <$> between (string "__") (string "__") inline)
                <|> try (StrongEmphasis <$> between (string "*") (string "*") inline)
                <|> try (StrongEmphasis <$> between (string "**") (string "**") inline)
                <|> try (Strikethrough <$> between (string "~~") (string "~~") inline)

paragraphParser :: Parser Paragraph
paragraphParser = many inline <* endOfLine

-- http://spec.commonmark.org/0.20/#atx-header
headingParser :: Parser Component
headingParser = do
        try (Heading1 <$> (string "# " >> paragraphParser))
    <|> try (Heading2 <$> (string "## " >> paragraphParser))
    <|> try (Heading3 <$> (string "### " >> paragraphParser))
    <|> try (Heading4 <$> (string "#### " >> paragraphParser))
    <|> try (Heading5 <$> (string "##### " >> paragraphParser))
    <|> try (Heading6 <$> (string "###### " >> paragraphParser))

eol :: Parser ()
eol = void endOfLine

-- http://spec.commonmark.org/0.20/#horizontal-rules
horizontalRuleParser :: Parser Component
horizontalRuleParser = do
            (try $ matchHorizontalRule '*')
        <|> (try $ matchHorizontalRule '-')
        <|> (try $ matchHorizontalRule '_')
            where matchHorizontalRule c = do
                    _ <- string (replicate 3 c)
                    _ <- manyTill (oneOf [' ', c]) (oneOf "\n") 
                    return HorizontalRule 


componentParser :: Parser Component
componentParser = 
            (
            try headingParser
        <|> try horizontalRuleParser
            )
        <* skipMany (eol)

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

