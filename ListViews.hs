module ListViews where

import GoDefinitions
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
import Graphics.UI.Gtk.ModelView
import Graphics.UI.Gtk.Gdk.Events

import Control.Monad.Reader

import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Data.IORef


type TreeViewNet a = ReaderT a IO

type VgTreeViewNet = ReaderT VgTreeViewEnv IO
data VgTreeViewEnv = VgTreeViewEnv { vgEvt :: Event, 
				 wLChan :: TChan String, 
				 lLChan :: TChan String, 
				 rmStore :: ListStore Room,
				 window :: Window,
				 sobRef :: IORef (Maybe Int),
				 bRef :: IORef (Maybe Board)}

type VpTreeViewNet = ReaderT VpTreeViewEnv IO
data VpTreeViewEnv = VpTreeViewEnv { vpEvt :: Event,
				     plStore :: ListStore Player,
				     challDialog :: Dialog,
				     label :: Label}

intValue :: [Int] -> String -> Int
intValue [node] err = node
intValue _      err  = error err

vgViewMenu :: TreePath -> VgTreeViewNet ()
vgViewMenu treePath = do
        event <- asks vgEvt
	model <- asks rmStore
	viewGameWindow <- asks window
	wLoopChan <- asks wLChan
	lLoopChan <- asks lLChan
	sizeOfBoardRef <- asks sobRef
	boardRef <- asks bRef
	menu <- liftIO $ menuNew
	menuItem <- liftIO $ menuItemNewWithLabel "Show Game"
	liftIO $ onActivateLeaf menuItem $ do
			game <- listStoreGetValue model (intValue treePath "view game menu") 
			modifyIORef sizeOfBoardRef (\_ -> Just (sizeOfBoard game))
                        modifyIORef boardRef (\_ -> Just (emptyBoard (sizeOfBoard game) :: Board))
		        sendBoardInfo game wLoopChan lLoopChan  
		        widgetShow viewGameWindow
	liftIO $ menuShellAppend menu menuItem
	liftIO $ widgetShowAll menu
	liftIO $ menuPopup menu (Just ((eventButton event), (eventTime event)))

vpViewMenu treePath = do
	event <- asks vpEvt
	model <- asks plStore
	dialog <- asks challDialog
	lbl <- asks label
        menu <- liftIO $ menuNew
	menuItem <- liftIO $ menuItemNewWithLabel "Challenge"
	liftIO $ onActivateLeaf menuItem $ do	
			player <- listStoreGetValue model (intValue treePath "view player menu")
			liftIO $ labelSetText lbl ("you are challenging " ++ name player ++ " to a match")
			liftIO $ putStrLn (show player)
			widgetShow dialog
	liftIO $ menuShellAppend menu menuItem
	liftIO $ widgetShowAll menu
	liftIO $ menuPopup menu (Just ((eventButton event), (eventTime event)))

sendBoardInfo game wLChan lLChan = do
  if roomHandicap game /= 0 then liftIO $ atomically (writeTChan lLChan ("board size " ++ (show (sizeOfBoard game))))
  		      else return ()
  liftIO $ atomically (writeTChan wLChan ("moves " ++ gameId game))

treeViewCallBack :: (TreePath -> TreeViewNet a ()) -> TreeView -> Event -> TreeViewNet a Bool
treeViewCallBack callBackAction treeView event = do
	if eventButton event == RightButton then do
		selection <- liftIO $ treeViewGetSelection treeView
		selectedRows <- liftIO $ treeSelectionCountSelectedRows selection
		if selectedRows <= 1 then do
		  Just (treePath, _, _) <- liftIO $ treeViewGetPathAtPos treeView (round $ eventX event, round $ eventY event)
		  liftIO $ treeSelectionUnselectAll selection
		  liftIO $ treeSelectionSelectPath selection treePath
		  callBackAction treePath
		  return (True)
		  		     else do
         	  return (False)
	                                    else do
        	return (False)

setTitle = treeViewColumnSetTitle

vgTitle n col = do
  case n of 
    1 -> setTitle col "white"
    2 -> setTitle col "rank"
    3 -> setTitle col "black"
    4 -> setTitle col "rank"
    5 -> setTitle col "move"

vpTitle n col = do
  case n of 
    1 -> setTitle col "name"
    2 -> setTitle col "rank"
    3 -> setTitle col "idle"
    4 -> setTitle col "room obs"
    5 -> setTitle col "room play"
    6 -> setTitle col "quiet or shout"
    7 -> setTitle col "active"

setAttribute model c r prop = cellLayoutSetAttributes c r model $ \row -> [ cellText := prop row ]

vgAttribute model n (col,rend) = do
  case n of 
    1 -> setAttribute model col rend whiteName
    2 -> setAttribute model col rend wrk 
    3 -> setAttribute model col rend blackName 
    4 -> setAttribute model col rend brk
    5 -> setAttribute model col rend move

vpAttribute model n (col,rend) = do
  case n of
    1 -> setAttribute model col rend name 
    2 -> setAttribute model col rend rank
    3 -> setAttribute model col rend idle 
    4 -> setAttribute model col rend roomObs
    5 -> setAttribute model col rend roomPlay
    6 -> setAttribute model col rend annFlag 
    7 -> setAttribute model col rend lookFlag 

setupView view model numOfCols titles attributes = do
  treeViewSetHeadersVisible view True
  rendererList <- mapM (const cellRendererTextNew) [1..numOfCols]
  colList <- mapM (const treeViewColumnNew) [1..numOfCols]
  zipWithM_ (titles) [1..numOfCols] colList
  zipWithM_ (\c r -> treeViewColumnPackStart c r True) colList rendererList
  zipWithM_ (attributes model) [1..numOfCols] (zip colList rendererList)
  mapM_ (treeViewAppendColumn view) colList
  treeViewSetModel view model

setupViewOfGame view model = do
  setupView view model 5 vgTitle vgAttribute

setupViewOfPlayer view model = do
  setupView view model 7 vpTitle vpAttribute
