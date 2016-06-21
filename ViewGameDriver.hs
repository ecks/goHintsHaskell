module ViewGameDriver where

import GoDefinitions
import ViewGame
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Data.IORef

blackStone = "black_stone.png"

whiteStone = "white_stone.png"

main = do
	initGUI
	Just xml <- xmlNew "goClient.glade"
	testWindow <- xmlGetWidget xml castToWindow "window3"
	canvas <- xmlGetWidget xml castToDrawingArea "drawingarea1"
        widgetSetName canvas "mywidget"
        boardRef <- newIORef (Just (filledBoard 19 W 0 fullBoard :: Board))
        sizeOfBoardRef <- newIORef (Just 19)
        bStone <- pixbufNewFromFile blackStone
        wStone <- pixbufNewFromFile whiteStone
        testWindow `onDestroy` mainQuit
	canvas `onExpose` updateCanvas canvas sizeOfBoardRef boardRef bStone wStone
 	widgetShow testWindow
	mainGUI
	
fullBoard = concat $ map (\n -> map (\m -> Square (n,m) B) [1..19]) [1..19]
