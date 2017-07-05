module SourceBrowser where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
import Graphics.UI.Gtk.SourceView
import Control.Monad
import Data.Tree (Tree)
import qualified Data.Tree as Tree

import Language.C.Data.Position
import Language.C.Data.Node
import GenericTree

runGTK :: [Tree AstNode] -> FilePath -> IO ()
runGTK tree file = do
  initGUI

  win <- windowNew
  onDestroy win mainQuit
  (model,treeview) <- createTreeView tree
  (buffer,sourceview, sourcewin) <- createSourceView file
  -- select text based on node
  New.onCursorChanged treeview $ do
    (path,_) <- New.treeViewGetCursor treeview
    let selector = getSelector tree path
    newPos <- selectText buffer (getOffset selector) (getLength selector)
    -- scroll (textviewScrollToIter didn't work properly on my machine)
    case newPos of 
        Just p ->
            do mark <- textBufferCreateMark buffer Nothing p False
               textViewScrollToMark sourceview mark 0 (Just (1.0,0.20))
        _ -> return ()
  sTreeView <- wrapScrolled treeview (PolicyNever, PolicyAutomatic)  
  box <- hBoxNew False 5
  boxPackStart box sTreeView PackNatural 5
  boxPackEnd box sourcewin PackGrow 5
  containerAdd win box
  windowSetDefaultSize win 1024 768
  widgetShowAll win
  mainGUI

-- select text
selectText :: (TextBufferClass self) => self -> Maybe Int -> Maybe Int -> IO (Maybe TextIter)
selectText _buffer Nothing _ = return Nothing
selectText buffer (Just offs) mLength = do
    start <- textBufferGetStartIter buffer
    textIterSetOffset start offs
    case mLength of
        Just l -> do
            end <- textIterCopy start
            textIterForwardChars end l
            textBufferSelectRange buffer start end
        _ -> textBufferPlaceCursor buffer start
    return $ Just start

createSourceView :: FilePath -> IO (TextBuffer, TextView, ScrolledWindow)
createSourceView src = do
  -- create text buffer and view
  buffer <- textBufferNew Nothing
  -- load up and display a file
  fileContents <- readFile src
  textBufferSetText buffer fileContents
  textBufferSetModified buffer False

  sv <- textViewNewWithBuffer buffer
  sw <- wrapScrolled sv (PolicyAutomatic,PolicyAutomatic)
  return (buffer,sv,sw)

wrapScrolled :: (WidgetClass widget) => widget -> (PolicyType,PolicyType) -> IO ScrolledWindow
wrapScrolled w (phor,pver)= do
  -- put it in a scrolled window
  sw <- scrolledWindowNew Nothing Nothing
  sw `containerAdd` w
  scrolledWindowSetPolicy sw phor pver
  sw `scrolledWindowSetShadowType` ShadowIn
  return sw

createTreeView :: Tree.Forest AstNode -> IO (New.TreeStore AstNode, New.TreeView)
createTreeView forest = do
  -- create a new tree model
  model <-   New.treeStoreNew forest
  view <- New.treeViewNewWithModel model
  -- enable headers
  New.treeViewSetHeadersVisible view True

  -- add three columns
  cols@[col1,col2,col3] <- replicateM 3 New.treeViewColumnNew
  -- set labels
  zipWithM New.treeViewColumnSetTitle cols (words "Label Start Length")
  -- create renderers
  renderers@[renderer1,renderer2,renderer3] <- replicateM 3 New.cellRendererTextNew
  zipWithM_ (\a b -> New.cellLayoutPackStart a b True) cols renderers
  -- program renderers
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := show row ]
  New.cellLayoutSetAttributes col2 renderer2 model $ \row -> [ New.cellText := maybe "" show (getOffset row) ]
  New.cellLayoutSetAttributes col3 renderer3 model $ \row -> [ New.cellText := maybe "" show (getLength row) ]
  -- append columns
  mapM_ (New.treeViewAppendColumn view) cols
  return (model,view)

getOffset :: AstNode -> Maybe Int
getOffset node = pos >>= getOffs
    where
    pos = case node of
             (AstNode _ _ (Just ni)) -> Just (posOf ni)
             IdentNode ident -> Just (posOf ident)
             ConstNode cconst -> Just (posOf cconst)
             _ -> Nothing
    getOffs p = fmap posOffset (ensure isSourcePos p)

ensure :: (a -> Bool) -> a -> Maybe a
ensure p x = if p x then Just x else Nothing

getLength :: AstNode -> Maybe Int
getLength (AstNode _ _ (Just ni)) = lengthOfNode ni
getLength (IdentNode ident) = lengthOfNode (nodeInfo ident)
getLength (ConstNode cconst) = lengthOfNode (nodeInfo cconst)
getLength _ = Nothing

getSelector :: [Tree a] -> [Int] -> a
getSelector [] [] = error "getSelector: unreachable tree element"
getSelector (t:ts) [] = Tree.rootLabel t
getSelector trees (t:ts) = get' (trees !! t) ts where
    get' tree [] = Tree.rootLabel tree
    get' tree (t:ts) = get' (Tree.subForest tree !! t) ts


   -- create the appropriate language
  -- lm <- sourceLanguagesManagerNew
  -- langM <- sourceLanguagesManagerGetLanguageFromMimeType lm "text/x-haskell"
  -- lang <- case langM of
  --   (Just lang) -> return lang
  --   Nothing -> do
  --     langDirs <- sourceLanguagesManagerGetLangFilesDirs lm
  --     error ("please copy haskell.lang to one of the following directories:\n"
  --         ++unlines langDirs)
