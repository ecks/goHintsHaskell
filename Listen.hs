module Listen where

import GoDefinitions
import GetParsed
import Handicap

import Graphics.UI.Gtk hiding (Color)
import Graphics.UI.Gtk.ModelView as MV

import Text.Regex.Posix
import Text.Printf

import Control.Monad.Reader
import qualified Control.Monad.State as State
import Control.Monad.Error  
import Control.Applicative hiding ((<|>))

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan

import Data.Maybe
import Data.IORef
import Data.List
import System.IO 
--import System.IO.UTF8 as UTF

type ListenNet = ReaderT ListenEnv (ErrorT String (State.StateT Stage IO))
data ListenEnv = ListenEnv { lChan :: TChan String, 
			     textBuffer :: TextBuffer, 
			     viewGameStore :: MV.ListStore Room,
			     viewPlayerStore :: MV.ListStore Player,
			     viewGameWindow :: Window,
			     bRef :: IORef (Maybe Board)}

type StageNet = State.StateT Stage IO
data Stage = Head | Login | Pass | Motd | Text | Rooms | PeopleList | Ready | RoomView deriving Eq

-- this function blocks, if it can't get get anything from the handle
-- it pretty much reads from the network socket
listenLoop :: Handle -> ListenNet ()
listenLoop h = forever $ do
               s <- liftIO $ hGetLine h
               parseString s

parseString :: String -> ListenNet ()
parseString s = do
        liftIO $ putStrLn s
        stage <- State.get
        tb <- asks textBuffer
        vgStore <- asks viewGameStore
        vpStore <- asks viewPlayerStore
	boardRef <- asks bRef
        case stage of
          Head -> if ((s =~ "^Login:.*") :: Bool) then State.put Login else return ()
          Login -> if ((s =~ "^1 1.*") :: Bool) then State.put Pass else return ()
          Pass -> if ((s =~ "^9 File.*") :: Bool) then State.put Motd else return ()
          Motd -> if ((s =~ "^9 File.*") :: Bool) then State.put Text else liftIO $ postGUIAsync (textBufferInsertAtCursor tb s)

          Text -> if ((s =~ "^1 5.*") :: Bool) then State.put Rooms else return ()
          Rooms -> if ((s =~ "^2 5.*") :: Bool) then State.put PeopleList else 
                        if ((s =~ "^7 [[]##[]].*") :: Bool) then return () -- header which we can ignore
							else 
			let 
                            gameId = getParsed findGameId s
                            white = getParsed findWhite s 
                            wrk = getParsed findWrk s
                            black = getParsed findBlack s
                            brk = getParsed findBrk s
                            move = getParsed findMove s
                            size = getParsed findSize s
                            handi = getParsed findHandicap s
                        in case liftM8 Room gameId white wrk black brk move ((\a -> read a :: Int) <$> size) ((\a -> read a :: Int) <$> handi) of
                          Left err -> throwError ("error in parsing Room: " ++ err ++ " @ " ++ s)
                          Right r -> liftIO $ postGUIAsync (MV.listStoreAppend vgStore r >> return ())
          PeopleList -> if ((s =~ "^1 5.*") :: Bool) then State.put Ready else 
			  if ((s =~ "^27  Info [ ]*Name.*") :: Bool) then return () -- header which we can ignore
			     				else
                          if ((s =~ "^27 [ ]*[*][*]*.*") :: Bool) then return () -- footer which we can ignore
							else
			  let	
			      (_,_,str) = ((s =~ "^27") :: (String,String,String))
			      fStr = filter (/= '\r') str
			      (fstS, _, sndS) = ((fStr =~ "[|]") :: (String,String,String))
			      names = mapM (getParsed findName) [fstS,sndS]
                              ranks = mapM (getParsed findRank) [fstS,sndS]
                              idles = mapM (getParsed findIdle) [fstS,sndS]
                              roomObsvs = mapM (getParsed findRoomObs) [fstS,sndS]
                              roomPlays = mapM (getParsed findRoomPlay) [fstS,sndS]
			      annFlags = mapM (getParsed findAnnFlag) [fstS,sndS]
			      lookFlags = mapM (getParsed findLookFlag) [fstS,sndS]
			  in case (liftM7.zipWith7)  Player names ranks idles roomObsvs roomPlays annFlags lookFlags of
                            Left err -> if ((fstS =~ "^[ ]*(Logon|Logon |[ ])$") :: Bool) then return ()
					  else if ((sndS =~ "^[ ]*(Logon|Logon |[ ])$") :: Bool) then return ()
					    else liftIO (print ("fst: " ++ fstS)) >>
						 liftIO (print ("snd: " ++ sndS)) >>
						 throwError ("error in parsing PeopleList: " ++ err ++ " @ " ++ str)
                            Right [p1,p2] -> liftIO $ postGUIAsync (MV.listStoreAppend vpStore p1 >> 
								    MV.listStoreAppend vpStore p2 >> return ())
          Ready -> if ((s =~ "^15 Game.*") :: Bool) then State.put RoomView else return () 
          RoomView -> if ((s =~ "^15.*") :: Bool) then  
                        let
                                maybeColor = getParsed findThisColor s
                                maybeMoves = getParsed findThisMove s
                        in case return (,) `ap` maybeColor `ap` maybeMoves of
			  Left err -> throwError ("error in Parsing Color or Move")
                          Right (color,moves) -> if ((moves =~ "^Handicap.*") :: Bool) then (processHandicap colorType moves) 
			  			    else case getParsed getAllMoves moves of
							   Left err -> throwError ("error in Parsing Moves: " ++ err ++ " @ " ++ moves)
							   Right xL -> (processMove.map (\move -> Square (fromGoCoordToTuple move) colorType)) xL
						    where
						      colorType = (read color :: Color)

										    --liftIO (atomically (writeTChan listenChan "Done")) >>
                                                  else if ((s =~ "^1 5.*") :: Bool) then State.put Ready
                                                                                    -- we ignore the comment about handicaps and komi for now
                                                                                    else if ((s =~ "^9.*") :: Bool) then return ()
                                                                                                                    else return ()

 
liftM7 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m r
liftM7 r a b c d e f g = return r `ap` a `ap` b `ap` c `ap` d `ap` e `ap` f `ap` g  

liftM8 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m r
liftM8 r a b c d e f g h = return r `ap` a `ap` b `ap` c `ap` d `ap` e `ap` f `ap` g `ap` h 

--zipWithM7 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> m r) -> [a1] -> [a2] -> [a3] -> [a4] -> [a5] -> [a6] -> [a7] -> m [r]
--zipWithM7 f tl ul vl wl xl yl zl = sequence (zipWith7 f tl ul wl xl yl zl)

fromGoCoordToTuple :: String -> (Int,Int)
fromGoCoordToTuple str = (fromJust ((lookup.head) str $ zip (['A'..'H'] ++ ['J'..'T']) [1..19]),(read.tail) str :: Int)

processHandicap :: Color -> String -> ListenNet ()
processHandicap color move = do
  listenChan <- asks lChan
  boardRef <- asks bRef 
  window <- asks viewGameWindow
  board <- liftIO $ readIORef boardRef
  flakySize <- liftIO $ atomically (readTChan listenChan)
  liftIO $ print ("from processHandicap: " ++ flakySize)
  if ((flakySize  =~ "^board size.*") :: Bool) then 
    let boardSize = (read ((flakySize =~ "[0-9]*$") :: String) :: Int)
        numOfHandicap = (read ((move =~ "[0-9]$") :: String) :: Int) 
    in case (getHandicap numOfHandicap boardSize) of
       Left "error" -> throwError "error finding handicap"
       Right hc -> do
         liftIO $ (modifyIORef boardRef (\(Just board) -> Just (foldr (\h b -> setPiece b  (Square h color)) board hc)))  
         liftIO $ postGUIAsync (widgetQueueDraw window)
                                               else 
						      throwError "flaky board size"

processMove squareList = do
  boardRef <- asks bRef
  window <- asks viewGameWindow
  if length squareList > 1 then do
  		liftIO $ modifyIORef boardRef (\(Just board) -> Just (foldr (\(Square coord col) b -> unsetPiece b (Square coord (getOtherColor col))) 
										board (tail squareList)))
  			   else return ()
  liftIO $ modifyIORef boardRef (\(Just b) -> (Just (setPiece b (head squareList))))
  liftIO $ postGUIAsync (widgetQueueDraw window)
