{-# LANGUAGE InstanceSigs #-}

-- | Module for defining lambda calculus grammar.
module Parser.Grammar where
    
import Parser.Parser
import Control.Applicative ((<|>))
import Data.Char

-- | For clarity
type Parameters = [Token]

-- | Special Tokens
data Token = Lambda Char | Period Char | Identifier Char

-- | For pretty printing of Tokens
instance Show Token where
    show :: Token -> String
    show (Lambda c) = [c]
    show (Period p) = [p]
    show (Identifier s) = [s]

parseToken :: Parser Token
parseToken = parseLambda <|> parsePeriod <|> parseIdentifier

parseLambda :: Parser Token
parseLambda = Lambda <$> char '\\'

parsePeriod :: Parser Token
parsePeriod = Period <$> char '.'

parseIdentifier :: Parser Token
parseIdentifier = Identifier <$> satisfies isLetter

-- | The main expression type defining lc grammar.
data Expr = Wrapped Expr | Literal Token | Function Parameters Expr | Application Expr Expr

-- | For pretty printing expressions
instance Show Expr where
    show :: Expr -> String
    show (Literal t) = show t
    show (Function params expr) = "λ" ++ (concatMap show params) ++ "." ++ (show expr)
    show (Application lexpr rexpr) = "[" ++ (show lexpr) ++ " " ++ (show rexpr) ++ "]"
    show (Wrapped expr) = "(" ++ (show expr) ++ ")"

parseExpr :: Parser Expr
parseExpr = parseApplication <|> parseFunction <|> parseLiteral <|> parseWrapped <|> failure "Invalid LC expression"

parseWrapped :: Parser Expr
parseWrapped = Wrapped <$> parens parseExpr

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
    fn <- parseWrapped <|> parseFunction <|> parseLiteral
    spacing
    arg <- parseExpr
    return $ Application fn arg