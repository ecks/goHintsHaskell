module Handicap where

import Text.Regex.Posix

import Control.Monad.Trans
import Control.Monad.STM
import Control.Monad.Reader
import Control.Applicative
import Control.Concurrent.STM.TChan

import GoDefinitions

mid :: Int -> Int
mid boardSize = ceiling $ (realToFrac boardSize) / 2

three boardSize = if boardSize > 11 then 4 else 3

places =      [(2,2),(-2,-2),(-2,2),(2,-2), -- first four handicaps
        				    -- fifth will be placed explicitly
	       (-2,0), (2,0),               -- two places for sixth
	       				    -- seventh will be placed explicitly
	       (0,2), (0,-2),               -- two places for eight
	       (0,0)]                       -- finally ninth place

getHandicap :: Int -> Int -> Either String [(Int,Int)]
getHandicap hc bs = if hc == 5 || hc == 7 then ((mid bs,mid bs):) <$> (findRestOfHandicap (hc-1)) else findRestOfHandicap hc 
		where findRestOfHandicap hcN = if hcN < 2 || hcN > 9 then Left "error" else
					       Right $ Prelude.map ((\(a,j) -> if j == 2 then (a,bs - three bs + 1) else if j == 0 then (a,mid bs) else (a,three bs)) . 
							(\(i,b) -> if i == 2 then (bs - three bs + 1,b) else if i == 0 then (mid bs,b) else (three bs,b)))
									(take hcN places)


