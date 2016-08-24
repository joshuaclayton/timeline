module Timeline.Parser.Internal
    ( parseOnly
    , double
    , brackets
    , parens
    ) where

import           Control.Monad (void)
import qualified Data.Bifunctor as BF
import           Data.Scientific (toRealFloat)
import           Data.Text (Text)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

parseOnly :: Parser a -> Text -> Either String a
parseOnly p = BF.first parseErrorPretty . runParser p ""

double :: Parser Double
double = L.signed sc $ toRealFloat <$> lexeme L.number

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser () -- space consumer
sc = L.space (void spaceChar) lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"
