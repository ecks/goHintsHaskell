module GetParsed where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

getParsed p s = case (parse p "" s) of
	Left err -> Left (show err)
	Right x -> Right x

{- I'm not quite sure yet what it means 
   when a player has a * next to his name or 
   his rank, I've decided to ignore it for now
   until I know for sure. -}
{------------------------------------------------  Rooms state -----------------------------------------} 
findrk = do
	  space <|> do
	  	char '*' 
		space
	  char '['
	  spaces
	  many1 (alphaNum)


findGameId = do
	  digit 
	  space 
	  char '['
	  spaces 
	  many1 digit

findWhite = do
	  findGameId
	  char ']'
	  spaces
	  many1 (alphaNum)

findWrk = do
	  findWhite
	  findrk

findBlack = do
	  findWrk
	  space <|> char '*'
	  char ']'
	  space
	  string "vs."
	  spaces
	  many1 (alphaNum) 

findBrk = do
	  findBlack
	  findrk

findMove = do
	  findBrk
	  space <|> char '*'
	  char ']'
	  space
	  char '('
	  spaces
	  many1 (digit)

findSize = do
	  findMove
	  spaces
	  many1 (digit)

findHandicap :: GenParser Char st String
findHandicap = do
	  findSize
	  spaces
	  d <- digit
	  return ([d])

{------------------------------------------ PeopleList state ---------------------------------------}


findName :: GenParser Char st String
findName = do
	findRoomPlay
	many1 alphaNum

findRank :: GenParser Char st String
findRank = do
	findIdle
	spaces
	many1 alphaNum

findIdle :: GenParser Char st String
findIdle = do
	findName
	spaces <|> do
	  char '*'
	  spaces
	s <- many1 digit
        c <- (char 's' <|> char 'm')
	return (s ++ [c])

findRoomObs :: GenParser Char st String
findRoomObs = do
	findLookFlag
	spaces
	manyTill anyChar space 

findRoomPlay :: GenParser Char st String
findRoomPlay = do
	findRoomObs	
	spaces
	manyTill anyChar space

findAnnFlag :: GenParser Char st String
findAnnFlag = do
        space
	space
	c <- anyChar
	return ([c])

findLookFlag :: GenParser Char st String
findLookFlag = do
	findAnnFlag
        c <- anyChar
	return ([c]) 


{------------------------------------------ RoomView state ------------------------------------------}
findThisColor :: GenParser Char st String
findThisColor = do
	  digit
	  digit
	  spaces
	  many1 (digit)
	  char '('
	  l <- letter
	  return ([l])

findThisMove :: GenParser Char st String 
findThisMove = do
	  findThisColor
	  char ')'
	  char ':'
	  space
	  (try findThisHandicap <|> many1 (alphaNum <|> space))

-- helper function for findThisMove
findThisHandicap :: GenParser Char st String
findThisHandicap = do
	 str <- string "Handicap"
	 sp <- space
	 dig <- many1 (digit)
	 return (str ++ [sp] ++ dig)

{-- parsing multiple moves --}
getAllMoves :: GenParser Char st [String]
getAllMoves = do
	endBy1 (many1 alphaNum) space
