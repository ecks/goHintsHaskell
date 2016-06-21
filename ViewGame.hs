module ViewGame where

import GoDefinitions 
import GetParsed

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Control.Applicative

import Text.Regex.Posix
import Data.IORef
import Data.Maybe

import Graphics.UI.Gtk 
import Graphics.UI.Gtk.Gdk.Events

--updateCanvas :: DrawingArea -> IORef (Maybe Int) -> IORef (Maybe Board ) -> Pixbuf -> Pixbuf -> Event -> IO Bool
updateCanvas canvas sizeOfBoardRef boardRef bStone wStone (Expose {eventRegion = region }) = do
    maybeSizeOfBoard <- readIORef sizeOfBoardRef
    case maybeSizeOfBoard of
      Nothing -> return False
      Just sizeOfBoard -> do
              win <- widgetGetDrawWindow canvas
	      gc <- gcNewWithValues win $ newGCValues {
	        lineWidth = 3,
                capStyle = CapProjecting
              }
              gcSetClipRegion gc region
              (cWidth, cHeight) <- widgetGetSize canvas
              -- the initial size of the pixbufs
              sWidth' <- pixbufGetWidth bStone
              sHeight' <- pixbufGetHeight bStone
              (bStone,wStone) <- getScaledPixbuf (bStone,wStone) (sWidth',sHeight') (cWidth,cHeight)
              -- scaled size of the pixbufs
	      sWidth <- pixbufGetWidth bStone
              sHeight <- pixbufGetHeight bStone
              mapM_ (\n -> do
			drawLines win gc [(round $ squareSize cWidth sizeOfBoard,                     round $ squareSize cHeight sizeOfBoard * realToFrac n),
					  (round $ realToFrac cWidth - squareSize cWidth sizeOfBoard, round $ squareSize cHeight sizeOfBoard * realToFrac n)]
			drawLines win gc [(round $ squareSize cWidth sizeOfBoard * realToFrac n, round $ squareSize cHeight sizeOfBoard),
					  (round $ squareSize cWidth sizeOfBoard * realToFrac n, round $ realToFrac cHeight - squareSize cHeight sizeOfBoard)])
		    [1..(sizeOfBoard)]
              getMoves cWidth cHeight sWidth sHeight sizeOfBoard boardRef bStone wStone win gc
	      return True

squareSize cSize bSize = realToFrac cSize / realToFrac (bSize + 1)

getMoves :: Int -> Int -> Int -> Int -> Int -> IORef (Maybe Board) -> Pixbuf -> Pixbuf -> DrawWindow -> GC -> IO ()
getMoves cWidth cHeight sWidth sHeight bSize boardRef bS wS win gc = do
  maybeBoard <- readIORef boardRef 
  maybe (return ()) (\board -> mapM_ (\(Square (row,column) color) -> do
  		                 case color of
		                  B -> drawPixStone cWidth cHeight sWidth sHeight bSize win gc bS row column
		                  W -> drawPixStone cWidth cHeight sWidth sHeight bSize win gc wS row column)
		               (squares board)) 
		    maybeBoard 

getScaledPixbuf (bStone,wStone) (sWidth,sHeight) (cWidth,cHeight) = do
  sbStone <- pixbufScaleSimple bStone (round $ scaleRatio cWidth sWidth :: Int) (round $ scaleRatio cHeight sHeight :: Int) InterpNearest
  swStone <- pixbufScaleSimple wStone (round $ scaleRatio cWidth sWidth :: Int) (round $ scaleRatio cHeight sHeight :: Int) InterpNearest
  return (sbStone,swStone)


scaleRatio sSize iSize = realToFrac sSize / realToFrac iSize * (10)

drawPixStone cWidth cHeight sWidth sHeight bSize win gc stone row column = do
  drawPixbuf win gc stone 0 0 (round $ squareSize cWidth bSize * realToFrac row - realToFrac sWidth / 2) 
				(round $ realToFrac cHeight - squareSize cHeight bSize * realToFrac column - realToFrac sHeight / 2)
		(-1) (-1) (RgbDitherNone) 0 0
        


{-- drawStone cWidth cHeight bSize sWidth sHeight stone row column = do
   identityMatrix
   translate (squareSize cWidth bSize * realToFrac row - (realToFrac sWidth * scaleRatio cWidth sWidth / 2)) 
   			(realToFrac cHeight - squareSize cHeight bSize * realToFrac column - (realToFrac sHeight * scaleRatio cHeight sHeight / 2)) 
   scale ( scaleRatio cWidth sWidth ) ( scaleRatio cHeight sHeight )
   setSourceSurface stone 0 0
--   paint  --}

