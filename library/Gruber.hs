module Gruber (module Gruber) where

import Control.Applicative ((<$>), (<*), (<|>))
import Control.Monad (void)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim (many, parse, try)
import Text.Parsec.String

data Inline =
        Emphasis String |
        StrongEmphasis String |
        Code String |
        NotFormatted String
        deriving (Eq, Show, Ord)

type Paragraph = [Inline]

data Component =
                Heading1 Paragraph |
                Heading2 Paragraph |
                Heading3 Paragraph |
                Heading4 Paragraph |
                Heading5 Paragraph |
                Heading6 Paragraph
                deriving (Show, Eq, Ord)

type Document = [Component]

inline :: Parser Inline
inline = do
            try (Emphasis <$> (detectInline '_'))
        <|> try (StrongEmphasis <$> (detectInline '*'))
        <|> try (NotFormatted <$> (many1 (noneOf "\n_*")))
            where detectInline c = do
                    _ <- string [c]
                    t <- many1 (noneOf [c, '\n'])
                    _ <- string [c]
                    return t

paragraphParser :: Parser Paragraph
paragraphParser = many inline <* endOfLine

headingParser :: Parser Component
headingParser = do
        try (Heading1 <$> (string "# " >> paragraphParser))
    <|> try (Heading2 <$> (string "## " >> paragraphParser))
    <|> try (Heading3 <$> (string "### " >> paragraphParser))
    <|> try (Heading4 <$> (string "#### " >> paragraphParser))
    <|> try (Heading5 <$> (string "##### " >> paragraphParser))
    <|> try (Heading6 <$> (string "###### " >> paragraphParser))

consumeEndOfLine :: Parser ()
consumeEndOfLine = void endOfLine

componentParser :: Parser Component
componentParser = headingParser <* (try consumeEndOfLine <|> try eof)

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

