type Parser a = String -> [(a,String)] 

returns :: a -> Parser a
returns v = \ input -> [(v,input)]

fails :: Parser a
fails = \ input -> []

items :: Parser Char
items = \ input -> case input of
                         [] -> []
                         (x:xs) -> [(x,xs)]

parse :: Parser a -> String -> [(a,String)]
parse p input = p input

binds :: Parser a -> (a -> Parser b) -> Parser b
binds theparser function = \string -> case (parse theparser string) of
 [] -> []
 [(parameter, result)] -> (parse (function parameter) result)

sequences :: Parser a -> Parser b -> Parser Int
sequences firstparser secondparser = 
 binds firstparser (\x ->
  (binds secondparser (\y ->
   (returns 1))))

thisparser :: Parser (Char, Char)
thisparser = binds items (\x ->
 (binds items (\y ->
  (returns (x,y)))))


orelse::Parser a -> Parser a -> Parser a
orelse thisparser thatparser = \string -> case (parse thisparser string) of
 [] -> parse thatparser string
 [(parameter, output)] -> [(parameter, output)]

itself :: Parser a -> Parser a
itself thisparser = binds thisparser (\x -> thisparser)

constantlyswears :: Parser a -> Parser String
constantlyswears thisparser = binds thisparser (\x -> returns "Fuck you!")

satisfaction :: (Char -> Bool) -> Parser Char
satisfaction predicate = binds items (\x ->
 if (predicate x) then (returns x) else fails)

-- various predicates
isDigit :: Char -> Bool
isDigit this = elem this ['0'..'9']

isLower:: Char -> Bool
isLower this = elem this ['a'..'z']

isUpper :: Char -> Bool
isUpper this = elem this ['A'..'Z']

isAlpha :: Char -> Bool
isAlpha this = (isLower this) || (isUpper this)

isAlphaNum :: Char -> Bool
isAlphaNum this  = (isAlpha this) || (isDigit this)

--end predicates

--define various parsers

digit :: Parser Char
digit = satisfaction isDigit

lower :: Parser Char
lower = satisfaction isLower

upper :: Parser Char
upper = satisfaction isUpper

letter :: Parser Char
letter = satisfaction isAlpha

alphanum :: Parser Char
alphanum = satisfaction isAlphaNum

char :: Char -> Parser Char
char letter = satisfaction (== letter)

stringparses :: String -> Parser String
stringparses [] = returns []
stringparses (x:xs) = binds (char x) (\y ->
 binds (stringparses xs) (\z ->
  returns (x:xs)))

iterateparse :: Parser a -> Parser [a]
iterateparse thisparser = orelse (iterateparse1 thisparser) (returns [])

iterateparse1 :: Parser a -> Parser [a]
iterateparse1 thisparser = binds thisparser (\v ->
 binds (iterateparse thisparser) (\vs ->
  returns (v:vs) ))











