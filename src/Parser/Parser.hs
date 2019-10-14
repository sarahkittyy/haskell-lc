{-# LANGUAGE InstanceSigs #-}

-- | Module with root parsing logic
module Parser.Parser where
    
import Control.Applicative hiding (some, many)

-- | The parser datatype, takes in a string and returns either Left error or Right (match, rest)
newtype Parser a = Parser { parse :: String -> Either String (a, String) }

-- | Runs a parser, outputting an error when met with one.
runParser :: Parser a -> String -> a
runParser p input = case parse p input of
                        Left err -> error $ err
                        Right (match, rest) -> match

-- | Parser functor instance
instance Functor Parser where
    -- | Applies a function to the result of a parser
    fmap :: (a -> b) -> (Parser a -> Parser b)
    fmap fn p = Parser $ \input -> case parse p input of
                                    Left err -> Left err
                                    Right (m, rest) -> Right (fn m, rest)

-- | Parser applicative instance 
instance Applicative Parser where
    -- | Parser instance of any type is just a parser that returns that value with the input
    pure :: n -> Parser n
    pure n = Parser $ \input -> Right (n, input)
    
    -- | Parse the input with fp to get the function, then fmap (<$>) that onto the next parser and the remaining input from parsing fp
    (<*>) :: Parser (a -> b) -> (Parser a -> Parser b)
    fp <*> p = Parser $ \input -> case parse fp input of
                                    Left err -> Left err
                                    Right (fn, rest) -> parse (fn <$> p) rest             
    
-- | A failure consists of an error message
failure :: String -> Parser a
failure err = Parser $ \input -> Left err
                                    
-- | Parser monad instance
instance Monad Parser where
    -- | Same as pure
    return :: n -> Parser n
    return = pure
    
    -- | Bind, the value matched from the first parser is given to the monadic function, and parsed with the leftover input from the first parse
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= fn = Parser $ \input -> case parse p input of
                Left err -> Left err
                Right (match, rest) -> parse (fn match) rest
                
-- | If one parser fails, allow an alternate parser to run
instance Alternative Parser where
    -- | Empty parser is just a generic failure
    empty :: Parser a
    empty = failure "Parser error"
    
    -- | If the main parser fails, parse using the alternate parser
    (<|>) :: Parser a -> Parser a -> Parser a
    main <|> alt = Parser $ \input ->
                    case parse main input of
                        Left err -> parse alt input
                        res -> res

-- | Matches a single character
item :: Parser Char
item = Parser $ \input ->
                    if length input /= 0
                        then Right (head input, tail input)
                        else Left "No input left to consume for item."

-- | One or more
some :: Parser a -> Parser [a]
some parser = (:) <$> parser <*> rest
                        where
                            rest = some parser <|> pure []
-- | Zero or more
many :: Parser a -> Parser [a]
many parser = some parser <|> pure []

-- | Parser that matches a char that passes some predicate
satisfies :: (Char -> Bool) -> Parser Char
satisfies pred = item >>= \ch -> if pred ch
                                    then pure ch
                                    else failure $ "Predicate not satisfied with character " ++ [ch] ++ "."

-- | Matches a single, specific char
char :: Char -> Parser Char
char ch = satisfies (==ch)

-- | Matches a specific string
string :: String -> Parser String
string "" = pure ""
string (c:cs) = do
    char c
    string cs
    return (c:cs)

-- | Matches a single character, taken from the given list of possible characters.
oneOf :: [Char] -> Parser Char
oneOf chs = satisfies (flip elem chs)

-- | Matches n of the given parser
nOf :: Int -> Parser a -> Parser [a]
nOf n p = do
    if n == 0
        then return []
        else (:) <$> p <*> nOf (n - 1) p

-- | Matches a parser wrapped in parens
parens :: Parser a -> Parser a
parens p = do
    char '('
    res <- p
    char ')'
    return res