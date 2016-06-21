module Main where

import Listen hiding (bRef)
import ViewGame
import GoDefinitions
import ListViews

import Graphics.UI.Gtk hiding (
	ListStore,
 	listStoreNew,
	treeViewGetSelection,
        treeSelectionCountSelectedRows,
	treeViewGetPathAtPos,
        treeSelectionUnselectPath,
        treeSelectionUnselectAll,
        treeSelectionSelectPath,
	TreePath,
        treeViewSetHeadersVisible,
	cellRendererTextNew,
	treeViewColumnNew,
	treeViewColumnSetTitle,
	treeViewColumnPackStart,
	cellText,
        treeViewAppendColumn,
	treeViewSetModel)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView  
import Network 
--import System.IO.UTF8 as UTF
import System.IO 
import System.Exit

import Text.Printf

import Control.Monad.Reader
import qualified Control.Monad.State as State
import Control.Monad.Writer
import Control.Monad.Error
import Control.OldException

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan

import Data.IORef
import Prelude hiding (catch)


server = "igs.joyjoy.net"
port = 7777

blackStone = "black_stone.png"

whiteStone = "white_stone.png"

type WriteNet = ReaderT WriteEnv IO
data WriteEnv = WriteEnv { bot :: Bot, wChan :: TChan String}

data Bot = Bot { socket :: Handle }

connect :: TChan String -> TextBuffer -> ListStore Room -> ListStore Player -> Window -> IORef (Maybe Board) -> IO (Bot,ThreadId)
connect lChan tb vgStore vpStore vgWindow vgBoardRef = notify $ do
	h <- connectTo server (PortNumber (fromIntegral port))
	hSetBuffering h NoBuffering
	tid <- forkIO $ do
		r <- (State.evalStateT (runErrorT (runReaderT (listenLoop h) (ListenEnv lChan tb vgStore vpStore vgWindow vgBoardRef))) Head)
		case r of
		  Left e  -> putStrLn (show e)
		  Right _ -> return ()

        return (Bot h,tid)
      where 
        notify a = bracket_
	  (printf "Connectiong to %s ... " server >> hFlush stdout)
	  (putStrLn "done.")
	  a

writeLoop :: WriteNet ()
writeLoop = do
  chan <- asks wChan
  username <- liftIO (atomically $ readTChan chan)
  password <- liftIO (atomically $ readTChan chan)
  write username
  write password
  --sock <- socket (asks bot
  --liftIO . forkIO . runReaderT (listenLoop tb sw) =<< ask 
  chanLoop 

write :: String -> WriteNet ()
write s = do
        h <- asks bot 
	liftIO $ hPutStr (socket h) (s ++ "\r\n")
	liftIO $ putStrLn s


chanLoop :: WriteNet ()
chanLoop = forever $ do
                  chan <- asks wChan
		  s <- liftIO $ atomically $ readTChan chan
		  write s

bracketLoop :: TChan String -> TChan String -> TextBuffer -> ListStore Room -> ListStore Player -> Window -> IORef (Maybe Board) -> IO ()
bracketLoop wLoopChan lLoopChan tb vgStore vpStore vgWindow vgBoardRef = bracket (connect lLoopChan tb vgStore vpStore vgWindow vgBoardRef) disconnect loop
                                where
				  disconnect (st,tid) = do
				  		hClose (socket st) 
						throwTo tid (ExitException ExitSuccess)
						putStrLn "Disconnected"
	                          loop (st,_) = catch (runReaderT (writeLoop) (WriteEnv st wLoopChan)) (const $ return ())
	                          
main :: IO ()
main = do
        {-- we will need three channels, one for writing to a thread that writes information to our socket,
	                                 one for writing to a thread that reads information from our socket,
					 and one for reading from a thread that reads information from our socket
	    We need two channels for one thread because the channels are not bi-directional so that if we try to send 
	    something on the same channel that is also used for listening, we will get back the information that we sent 
	    in the thread. --}
        wLoopChan <- atomically $ newTChan 
	lLoopChan <- atomically $ newTChan

	initGUI
	-- used for threading
	--timeoutAddFull (yield >> return True) priorityDefaultIdle 50
	Just xml <- xmlNew "goHints.glade"

        -- the window the user interacts with
	mainWindow <- xmlGetWidget xml castToWindow "window2"

	-- the first text window where the motd is displayed
	sw <- xmlGetWidget xml castToScrolledWindow "scrolledwindow1"
	tv <- xmlGetWidget xml castToTextView "textview1"
	tb <- textViewGetBuffer tv

	-- initialize the scroll-down menu
	connectItem <- xmlGetWidget xml castToMenuItem "Connect" 
	disconnectItem <- xmlGetWidget xml castToMenuItem "Disconnect" 
	quitItem <- xmlGetWidget xml castToMenuItem "Quit" 

	-- the connection window
	connectDialog   <- xmlGetWidget xml castToDialog "dialog1"
	okButton <- xmlGetWidget xml castToButton "button1"
	cancelButton <- xmlGetWidget xml castToButton "button2"
	usernameEntry <- xmlGetWidget xml castToEntry "entry1"
	passwordEntry <- xmlGetWidget xml castToEntry "entry2"

	-- initialization of the tree view for viewing games
	vgTreeView <- xmlGetWidget xml castToTreeView "treeview1"
	vgStore <- listStoreNew [] -- in the beginnig, our tree view contains nothing
	setupViewOfGame vgTreeView vgStore

        -- the drawing area used by pixbuf and family
	vgWindow <- xmlGetWidget xml castToWindow "window3"
	vgCanvas <- xmlGetWidget xml castToDrawingArea "drawingarea1"
        widgetSetName vgCanvas "mywidget"

        -- initialization of the tree view for viewing players
        vpTreeView <- xmlGetWidget xml castToTreeView "treeview2"
        vpStore <- listStoreNew []
	setupViewOfPlayer vpTreeView vpStore

	vpWindow <- xmlGetWidget xml castToWindow "window3"
	vpCanvas <- xmlGetWidget xml castToDrawingArea "drawingarea1"
	-- the dialog box that pops up when a player is challenged
	vpChallDialog <- xmlGetWidget xml castToDialog "dialog2"
        vpLabel <- xmlGetWidget xml castToLabel "label6"      

        bStone <- pixbufNewFromFile blackStone
        wStone <- pixbufNewFromFile whiteStone

        timeoutAdd (yield >> return True) 100
        
	vgBoardRef <- newIORef Nothing
        vgSobRef <- newIORef Nothing

        -- if you right-click on the tree view, you will be able to interact with a row
        vgTreeView `onButtonPress` (\e -> (runReaderT (treeViewCallBack vgViewMenu vgTreeView e) 
						  (VgTreeViewEnv e wLoopChan lLoopChan vgStore vgWindow vgSobRef vgBoardRef)))

	vpTreeView `onButtonPress` (\e -> (runReaderT (treeViewCallBack vpViewMenu vpTreeView e)
						  (VpTreeViewEnv e vpStore vpChallDialog vpLabel)))

	tid <- myThreadId
	tidRef <- newIORef tid
	-- at first we gray out the disconnect item
	widgetSetSensitivity disconnectItem False

	onActivateLeaf quitItem $ widgetDestroy mainWindow >> widgetDestroy connectDialog >> widgetDestroy vgWindow >> mainQuit 
        onActivateLeaf connectItem (widgetShow connectDialog)

	onActivateLeaf disconnectItem $ do
			  readIORef tidRef >>= \t -> throwTo t (ExitException ExitSuccess) >> putStrLn (show t)
			  -- since we have disconnected, the disconnect item should be grayed out
			  widgetSetSensitivity disconnectItem False
			  widgetSetSensitivity connectItem True

	onClicked okButton $ do
			   username <- get usernameEntry entryText
			   password <- get passwordEntry entryText
			   forkIO (withSocketsDo 
			   		(bracketLoop wLoopChan lLoopChan tb vgStore vpStore vgWindow vgBoardRef)) >>= 
					 \t -> writeIORef tidRef t >> putStrLn (show t)
			   atomically $ writeTChan wLoopChan username
			   atomically $ writeTChan wLoopChan password
			   -- since we have connected, disconnect should work now
			   widgetSetSensitivity disconnectItem True
			   widgetSetSensitivity connectItem False
			   widgetHide connectDialog
			   atomically $ writeTChan wLoopChan "toggle client true"
			   atomically $ writeTChan wLoopChan "games"
			   atomically $ writeTChan wLoopChan "who"


        vgCanvas `onExpose` (updateCanvas vgCanvas vgSobRef vgBoardRef bStone wStone)

	onClicked cancelButton $ do
	                         widgetHide connectDialog
	connectDialog `onDelete` (\ _ -> widgetHide connectDialog >>
				  return True)
	vgWindow `onDelete` (\ _ -> widgetHide vgWindow >>
				   return True)
	mainWindow `onDestroy` mainQuit
 
	widgetShow mainWindow
	mainGUI
