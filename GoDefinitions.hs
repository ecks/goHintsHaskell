module GoDefinitions (Color(..),
		      Square(..),
		      Board(..),
		      Room(..),
                      Player(..),
		      emptyBoard,
		      filledBoard,
		      setPiece,
		      unsetPiece,
		      getOtherColor)
  where

import Data.List
import Data.Maybe
import Control.Applicative

data Color = B | W deriving (Eq, Show, Read)
data Square = Square {
	coords :: (Int,Int),
	color :: Color 
	} deriving (Show, Eq, Read)

getOtherColor :: Color -> Color
getOtherColor W = B
getOtherColor B = W

data Board = Board {
	size :: Int,
	toMove :: Color,
	handicap :: Int,
	squares :: [Square]
	} deriving Show

data Room = Room { gameId :: String, whiteName :: String, wrk :: String, blackName :: String, brk :: String, move :: String, sizeOfBoard :: Int, roomHandicap :: Int } 
																			deriving Show

data Player = Player {name :: String, rank :: String, idle :: String, roomObs :: String, roomPlay :: String, annFlag :: String, lookFlag :: String}
																		deriving Show

class GoBoard b where
	emptyBoard :: Int -> b 
        filledBoard :: Int -> Color -> Int -> [Square] -> b
	setPiece :: b -> Square -> b
	unsetPiece :: b -> Square -> b

instance GoBoard Board where
	emptyBoard n = Board n B 0 []
        filledBoard n c h sL = Board n c h sL
	setPiece b s = b{squares=(squares b ++ [s])}
	unsetPiece b s = b{squares=(delete s (squares b))}
	--setHandicap b n p = (foldr (\square board -> setPiece board square p) 
	--						   b{handicap=n}) <$>    -- we need to convert foldr to use the Either data type
	--			                	   (fromTupleToSquare <$>) <$> (getHandicap n (size b)) -- same thing for a regular map
	--
