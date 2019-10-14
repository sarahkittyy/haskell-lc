-- | Module for defining lambda calculus grammar.
module Parser.Grammar where
    
import Parser.Parser
import Control.Applicative ((<|>))
import Data.Char

-- | For clarity
type Parameters = [Token]

-- | Special Tokens
data Token = Lambda Char | Period Char | Identifier Char deriving (Show)

parseToken :: Parser Token
parseToken = parseLambda <|> parsePeriod <|> parseIdentifier

parseLambda :: Parser Token
parseLambda = Lambda <$> char '\\'

parsePeriod :: Parser Token
parsePeriod = Period <$> char '.'

parseIdentifier :: Parser Token
parseIdentifier = Identifier <$> satisfies isLetter

-- | The main expression type defining lc grammar.
data Expr = Literal Token | Function Parameters Expr | Application Expr Expr deriving (Show)

parseExpr :: Parser Expr
parseExpr = parseFunction <|> parseApplication <|> parseLiteral

parseLiteral :: Parser Expr
parseLiteral = Literal <$> parseIdentifier

parseParameters :: Parser Parameters
parseParameters = some parseIdentifier

parseFunction :: Parser Expr
parseFunction = do
    parseLambda
    params <- parseParameters
    parsePeriod
    expr <- parseExpr
    return $ Function params expr
    
parseApplication :: Parser Expr
parseApplication = do
    fn <- parseFunction <|> parseLiteral
    char ' '
    arg <- parseApplication <|> parseLiteral
    return $ Application fn arg