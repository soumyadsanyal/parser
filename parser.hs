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

isSpace :: Char -> Bool
isSpace this = elem this [' ','\n','\t']


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

identifierparser :: Parser String
identifierparser = binds lower (\x ->
 binds (iterateparse alphanum) (\xs ->
  returns (x:xs)))

naturalnumberparser :: Parser Int
naturalnumberparser = binds (iterateparse1 digit) (\xs ->
 returns (read xs))

spaceparser :: Parser ()
spaceparser = binds (iterateparse (satisfaction isSpace)) (\x ->
 returns ())

tokenparser :: Parser a -> Parser a
tokenparser thisparser = 
 binds spaceparser (\x ->
  binds thisparser (\v ->
   binds spaceparser (\y ->
    returns v)))

identifierparser' :: Parser String
identifierparser' = tokenparser identifierparser

naturalnumberparser' :: Parser Int
naturalnumberparser' = tokenparser naturalnumberparser

symbolparser :: String -> Parser String
symbolparser symbol = tokenparser (stringparses symbol)

listofnumbersparser :: Parser [Int]
listofnumbersparser = 
 binds (symbolparser "[") (\x ->
  binds naturalnumberparser' (\n ->
   binds (iterateparse (shabang)) (\ns ->
    binds (symbolparser "]") (\z ->
     returns (n:ns)))))
   where
    shabang = 
     binds (symbolparser ",") (\a ->
      binds naturalnumberparser' (\b ->
       returns b))
 
-- We wish to parse the following grammar:
-- expr := term + expr | term
-- term := factor*term | factor
-- factor := (expr) | nat
-- nat := 0 | 1 | 2 | ...

expr :: Parser Int
expr = 
 binds term (\t ->
  (orelse
		  (binds (symbolparser "+") (\_ ->
		    binds expr (\e ->
		     returns (t+e))))
		  (returns t) ))


term = 
 binds factor (\f ->
  (orelse
		   (binds (symbolparser "*") (\_ ->
		     binds term (\t ->
		      returns (f*t))))
		  (returns f) ))


factor = orelse
		 (binds (symbolparser "(") (\_ ->
		  binds expr (\e ->
		   binds (symbolparser ")") (\_ ->
		    returns e))))
		 (naturalnumberparser')


eval :: String -> Int
eval xs = case (parse expr xs) of
 [(n,[])] -> n
 [(_,output)] -> error("unused input " ++ output)
 [] -> error "invalid input"



integerparser :: Parser Int
integerparser = 
 orelse
  startswithminusparser
  naturalnumberparser'
  where
   startswithminusparser = binds (tokenparser (char '-')) (\sign ->
    binds naturalnumberparser' (\number ->
     returns ((-1)*number) ))

isSentence :: Char -> Bool
isSentence this = (isAlphaNum this) || (isSpace this)

regularsentenceparser :: Parser Char
regularsentenceparser = satisfaction isSentence

commentparser :: Parser ()
commentparser =
 binds (tokenparser (char '-')) (\firstdash ->
  binds (tokenparser (char '-')) (\seconddash ->
   binds (iterateparse (regularsentenceparser)) (\line ->
    binds (tokenparser (char '\n')) (\endofline ->
     returns ()  ))))



