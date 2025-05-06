import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

number :: GenParser Char  st String 
number = many1 digit