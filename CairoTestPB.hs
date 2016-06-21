import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo
import Control.Monad
import Graphics.Rendering.Cairo.SVG


image = "june0611.png"

blackStone = "black_stone.png"
whiteStone = "white_stone.png"

boardSize :: Int 
boardSize = 19

scaleNum :: Int
scaleNum = 10


main = do
	initGUI
	Just xml <- xmlNew "goClient.glade"
	testWindow <- xmlGetWidget xml castToWindow "window3"
	canvas <- xmlGetWidget xml castToDrawingArea "drawingarea1"
        widgetSetName canvas "mywidget"
        bStone <- pixbufNewFromFile blackStone
        wStone <- pixbufNewFromFile whiteStone
        testWindow `onDestroy` mainQuit
	canvas `onExpose` \Expose { eventRegion = region } -> do
				drawTable canvas region bStone wStone
	onConfigure testWindow $ \Configure { eventWidth = w, eventHeight = h } -> do
	  putStrLn ("width is " ++ (show w) ++ " height is " ++ (show h))
--	  redrawStaticLayer canvas
	  return False
--	drawHandicaps canvas win (cWidth,cHeight)
        widgetShow testWindow
	mainGUI

drawTable canvas exposeRegion bStone wStone= do
--      withImageSurfaceFromPNG  blackStone $ \bs -> do
--       withImageSurfaceFromPNG whiteStone $ \ws -> do
 	  (cWidth, cHeight) <- widgetGetSize canvas
          (bStone,wStone) <- getScaledPixbuf (bStone,wStone) (cWidth,cHeight)
	  win <- widgetGetDrawWindow canvas 
          widgetName <- widgetGetName canvas
          putStrLn ("widget name is: " ++ widgetName)
          gc <- gcNew win
	  renderWithDrawable win $ do
	--        region exposeRegion
--                sWidth <- imageSurfaceGetWidth bs
--	        sHeight <- imageSurfaceGetHeight bs
		
--		liftIO $ putStrLn ("widget width is " ++ (show cWidth))
--		liftIO $ putStrLn ("widget height is " ++ (show cHeight))
--		liftIO $ putStrLn ("image width is " ++ (show sWidth))
--		liftIO $ putStrLn ("image height is " ++ (show sHeight))

--		withPatternForSurface surface $ \pattern -> do
--			patternSetExtend pattern ExtendRepeat
--			setSource pattern
--			paint
--			setSourceRGB 0 0 0
		        --setLineWidth 20
	        moveTo 0 0
                zipWithM (drawSquare cWidth cHeight boardSize) 
							(take ((boardSize-1)^2) (cycle [1..(boardSize-1)])) 
							(concat (map (replicate (boardSize-1)) [1..(boardSize-1)]))
	        stroke 
	  mapM (flip mapM [1..boardSize] . drawPixStone cWidth cHeight boardSize win gc bStone) [1..(boardSize)] 
    	  return True

--drawHandicaps canvas win (cWidth, cHeight) = case (getHandicap 2 boardSize) of
--			  Left _ -> return [()]
--			  Right hc -> renderWithDrawable win $ mapM (\(row,column) -> drawDot cWidth cHeight row column >> fill) hc

getScaledPixbuf (bStone,wStone) (cWidth,cHeight) = do
  bStoneWidth <- pixbufGetWidth bStone
  bStoneHeight <- pixbufGetHeight bStone
  wStoneWidth <- pixbufGetWidth wStone
  wStoneHeight <- pixbufGetHeight wStone
  sbStone <- pixbufScaleSimple bStone (round $ scaleRatio cWidth bStoneWidth :: Int) (round $ scaleRatio cHeight bStoneHeight :: Int) InterpNearest
  swStone <- pixbufScaleSimple wStone (round $ scaleRatio cWidth wStoneWidth :: Int) (round $ scaleRatio cHeight wStoneHeight :: Int) InterpNearest
  return (sbStone,swStone)

squareSize cSize bSize = realToFrac cSize / realToFrac (bSize + 1)
  
scaleRatio sSize iSize = realToFrac sSize / realToFrac iSize * (10)

drawPixStone cWidth cHeight bSize win gc stone row column = do
   drawPixbuf win gc stone 0 0 (row*40) (column*40) (-1) (-1) (RgbDitherNone) 0 0


drawStone cWidth cHeight bSize sWidth sHeight stone row column = do
   save
   translate (squareSize cWidth bSize * realToFrac row - (realToFrac sWidth * scaleRatio cWidth sWidth / 2)) 
   			(realToFrac cHeight - squareSize cHeight bSize * realToFrac column - (realToFrac sHeight * scaleRatio cHeight sHeight / 2)) 
   scale ( scaleRatio cWidth sWidth ) ( scaleRatio cHeight sHeight )
   setSourceSurface stone 0 0
   paint 
   restore

drawSquare cWidth cHeight bSize row column = do
   rectangle (squareSize cWidth bSize * realToFrac row) (squareSize cHeight bSize * realToFrac column) (squareSize cWidth bSize) (squareSize cHeight bSize)
