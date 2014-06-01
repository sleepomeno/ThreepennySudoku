{-# LANGUAGE CPP, RecursiveDo, RankNTypes, TemplateHaskell , PackageImports, NoMonomorphismRestriction #-}

module Main where

#ifdef CABAL
import qualified  "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
#endif
import Paths
import Reactive.Threepenny

import Data.List(groupBy,sort,isPrefixOf)
import Data.List.Split
import Data.Char(isDigit)
import Common
import Data.Maybe (listToMaybe, isJust, fromMaybe, fromJust)
import System.FilePath((</>),(<.>))
import Control.Monad(void,liftM,sequence,forM)
import Control.Monad.Trans.Class
import Data.Time
import Control.Arrow(first,second)
import Control.Monad.Trans.State as S
import Control.Lens ((^.),(&),Lens', makeLenses, view)
import Control.Lens.Getter (use)
import Control.Lens.Setter ((.=), (.~))
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO.Unsafe (unsafePerformIO)

data App = App { _easy :: [SudokuWithId],
                _medium :: [SudokuWithId],
                _hard :: [SudokuWithId],
                _insane :: [SudokuWithId],
                _window :: Window,
                _chosen :: SudokuWithId,
                _startTime :: UTCTime,
                _timer :: UI.Timer}
makeLenses ''App

type SudokuLens = Lens' App [SudokuWithId] 

main :: IO ()
main = do

    static <- getStaticDir
    let database = static </> db -- database file is in /wwwroot
    sudokus@[easySudokus, mediumSudokus, hardSudokus, insaneSudokus] <- readSudokus database

    let initialState w = App { _chosen = head easySudokus, _easy = easySudokus, _medium = mediumSudokus, _hard = hardSudokus, _insane = insaneSudokus, _window = w } 

        -- Run the application with the initial configuration
        executeApplication w = evalStateT mysetup $ initialState w

    -- startGUI defaultConfig
    --     { tpPort       =  10000
    --     , tpStatic     = Just static
    --     } executeApplication
    startGUI defaultConfig
        { tpPort       =  10000
        , tpStatic     = Just static
        } (runApp $ zip ["Easy", "Medium", "Hard", "Insane"] sudokus)

fromSQLRow [sqlId, sqlLevel, sqlContent] =
  let id = (fromSql sqlId) :: Integer
      level = (fromSql sqlLevel) :: String
      content = (fromSql sqlContent) :: String in
  Sudoku id (read level) (read content)

readSudokus :: FilePath -> IO [[SudokuWithId]]
readSudokus database  = do
  conn <- connectSqlite3 database
  rows <- quickQuery' conn "SELECT * FROM sudokus" []
  Database.HDBC.disconnect conn
  let sudokus = groupBy (\(Sudoku _ l1 _) (Sudoku _ l2 _) -> l1 == l2) $ sort $ map fromSQLRow rows
  return sudokus

mysetup :: StateT App UI ()
mysetup =  do
  win <- use window
  -- lift $ return (win C.# C.set title "Sudoku")
  lift $ UI.addStyleSheet win "sudoku.css"
  mytimer <- lift UI.timer
  lift $ UI.start mytimer
  timer .= mytimer
  now <- liftIO getCurrentTime
  startTime .= now
  showSudoku

-- selectSudoku' :: [SudokuWithId] -> [Event String]-> UI (UI.ListBox String, Event String)
-- selectSudoku' sudokus eListBoxes = mdo
--   let firstId = case head sudokus of { (Sudoku sid _ _) -> show sid }
--       choose = "--- choose a Sudoku ---"
--   listBox <- UI.listBox bItems bSelection bDisplayItem
--   let eSelection = rumors $ UI.userSelection listBox
--   bSelection <- stepper (Just choose) $ head <$> unions [eSelection, Just choose <$ unions eListBoxes]
--   let options = choose : map (\(Sudoku sid _ _) -> show sid) sudokus
--   bItems <- stepper options never :: UI (Behavior [String])
--   let bDisplayItem = pure UI.string :: Behavior (String -> UI Element)
--   -- bJustSelection <- stepper firstId (filterJust eSelection)
  
--   return (listBox, filterJust eSelection)


chooseSudokus = "--- choose a Sudoku ---"

selectSudokus' :: [[SudokuWithId]] -> UI [(UI.ListBox String, Event String)]
selectSudokus' sudokus = mdo
  listBox1 <- UI.listBox bItems1 bSelection1 bDisplayItem1
  listBox2 <- UI.listBox bItems2 bSelection2 bDisplayItem2
  let eSelection1 = rumors $ UI.userSelection listBox1
  let eSelection2 = rumors $ UI.userSelection listBox2
  bSelection1 <- stepper (Just chooseSudokus) $ head <$> unions [eSelection1, Just chooseSudokus <$ (filterE (all (/= chooseSudokus)) $ unions [filterJust eSelection2])]
  bSelection2 <- stepper (Just chooseSudokus) $ head <$> unions [eSelection2, Just chooseSudokus <$ (filterE (all (/= chooseSudokus)) $ unions [filterJust eSelection1])]
  let sudokus1 = map (\(Sudoku sid _ _) -> show sid) (sudokus!!0)
  let sudokus2 = map (\(Sudoku sid _ _) -> show sid) (sudokus!!1)
  let options1 = chooseSudokus : sudokus1
  let options2 = chooseSudokus : sudokus2
  -- bItems1 <- stepper options1 never :: UI (Behavior [String])
  bItems1 <- stepper options1 $ head <$> unions [sudokus1 <$ filterE (/= chooseSudokus) (filterJust eSelection1), options1 <$ (filterE (all (/= chooseSudokus)) $ unions [filterJust eSelection2])] :: UI (Behavior [String])
  bItems2 <- stepper options2 $ head <$> unions [sudokus2 <$ filterE (/= chooseSudokus) (filterJust eSelection2), options2 <$ (filterE (all (/= chooseSudokus)) $ unions [filterJust eSelection1])] :: UI (Behavior [String])
  let bDisplayItem1 = pure UI.string :: Behavior (String -> UI Element)
  let bDisplayItem2 = pure UI.string :: Behavior (String -> UI Element)
  
  return [(listBox1, filterJust eSelection1), (listBox2, filterJust eSelection2)]


selectSudoku'' :: [SudokuWithId] -> Behavior (Maybe String) -> UI (UI.ListBox String, Event String)
selectSudoku'' sudokus bSelection = mdo
  listBox <- UI.listBox bItems bSelection bDisplayItem
  let eSelection = rumors $ UI.userSelection listBox
  let options = chooseSudokus : map (\(Sudoku sid _ _) -> show sid) sudokus
  bItems <- stepper options never :: UI (Behavior [String])
  let bDisplayItem = pure UI.string :: Behavior (String -> UI Element)
  -- bJustSelection <- stepper firstId (filterJust eSelection)
  
  return (listBox, filterJust eSelection)

findSudoku :: [SudokuWithId] -> Integer -> SudokuWithId
findSudoku sudokus sid = head $ filter (\(Sudoku sid' _ _ ) -> sid' == sid) sudokus 

runApp :: [(String,[SudokuWithId])] -> Window -> UI ()
runApp sudokus'' w = void $ mdo
  UI.addStyleSheet w "sudoku.css"
  let fstSudokuId (Sudoku sid _ _:_) = show sid
      sudokus''' = take 2 sudokus''
  let (levels, sudokus') = unzip sudokus'''
  caption <- UI.h1

  let (listBoxes, eListBoxes) = unzip listBoxesAndEvents
  listBoxesAndEvents <- selectSudokus' sudokus' 
  
  let eSudoku' = filterE (/= chooseSudokus) $ head <$> unions eListBoxes
  bSudoku' <- stepper "0" eSudoku'
  let bSudoku''= fmap not <$> (==) <$> bSudoku' :: Behavior (String -> Bool)
      eSudoku = filterApply bSudoku'' eSudoku'

      
  bListBox <- (stepper Nothing $ Just <$> eSudoku) :: UI (Behavior (Maybe String))
  let bSudoku = fmap (findSudoku (concat sudokus') . read) <$> bListBox :: Behavior (Maybe SudokuWithId)
      bCaption = maybe "Welcome to Sudoku Land!" ("Sudoku "++) <$> bListBox

  cells <- sequence [createCell' col row digit eSudoku | row <- [0..8], col <- [0..8], let digit  = fmap ((!!col) . (!!row) . chunksOf 9 . extractDigits ) <$> bSudoku ]
  let (cellElems, eMistakes) = unzip $ fmap (first return) cells :: ([UI Element], [Event Bool])
  -- let cellElems = fmap (return . fst) cells
      
  let bChoose = (\x -> if isJust x then "welcome hide" else "welcome") <$> bListBox 
      bSudokuGrid = (\x -> if isJust x then "sudoku" else "sudoku hide") <$> bListBox
  choose <- UI.div #. "welcome" #+ [UI.h2 # set text "Choose a Sudoku ->"]
  element choose # sink UI.class_ bChoose
  content <- UI.div 
  element content # sink UI.class_ bSudokuGrid #+ sudokuGrid cellElems
  
  levelCaptions <- forM levels $ \x -> UI.h2 # set text x 

  selects <- forM  (zip3 listBoxes levelCaptions [0..3]) $
             \(listBox,level,nr) -> do
             let otherEvents = take nr eListBoxes ++ drop (nr+1) eListBoxes
             let ownEvent = eListBoxes!!nr
             let container = UI.div
             bWrapperClass <- stepper "selectSudokus" $ head <$> unions ["selectSudokus" <$ filterE (/= chooseSudokus) (head <$> unions otherEvents), "selectSudokus highlighted" <$ filterE (/= chooseSudokus) ownEvent]
             container # sink UI.class_ bWrapperClass #+ [element level, (element . getElement) listBox]
  element caption # sink text bCaption

  mistakes <- UI.h3
  bMistakesInt <- accumB 0 $ concatenate <$> unions [const 0 <$ eSudoku, (+1)  <$ unions eMistakes] :: UI (Behavior Int)
  let bMistakes = (++ " Mistakes") . show <$> bMistakesInt
  element mistakes # sink text bMistakes

  
  getBody w # set children ([caption, choose, content] ++ selects ++ [mistakes])


createCell' :: Int -> Int -> Behavior (Maybe Digit) -> Event String -> UI (Element, Event Bool)
createCell' x y digit eSudoku =
  let classes = "cell row"++show y++"col"++show x
      isFree digit' = case digit' of { (Guess _) -> True; _ -> False}
      digitString digit' = case digit' of
        (Guess _) -> ""
        (Free a) -> show a
      in
   do
     cell <- UI.div #. classes
     
     cellInput <- UI.entry (maybe "" digitString <$> digit) 
     -- digitValue <- currentValue digit
     -- (element . getElement) cellInput # set text (digitString digitValue) 
     element cellInput # sink UI.enabled (maybe False isFree <$> digit)

     (element . getElement) cellInput # set (attr "maxlength") "1"
     element cell # set children [getElement cellInput]
     let bDigit = UI.userText cellInput
         eInput = rumors bDigit
         isDigit' digit' = if length digit' /= 1 then False else isDigit $ head digit'
         eDigitInput = read <$> filterE isDigit' eInput :: Event Int
         eNonDigitInput = filterE (not . isDigit') eInput
         eCorrectState = (\x y -> maybe False (either (==y) (==y) . toEither) x) <$> digit <@> eDigitInput :: Event Bool
         eCorrect = filterE id eCorrectState
         eNotCorrect = filterE not eCorrectState
         
         eInputClasses = head <$> unions [classes ++ " wrong" <$ eNotCorrect, classes ++ " right" <$ eCorrect, classes <$ eNonDigitInput] 
     bInputClasses <- stepper classes $ head <$> unions [classes <$ eSudoku, eInputClasses]
     element cell # sink UI.class_ bInputClasses
     
     return (cell, eNotCorrect)
  
showSudoku :: StateT App UI ()
showSudoku  = do
  sdkToDisplay <- use chosen
  let (Sudoku sid _ _) = sdkToDisplay
  mytimer <- use timer
  mywindow <- use window
  mystartTime <- use startTime

  caption <- lift $ UI.h1 # set text ("Sudoku " ++ show sid)
  sudokus <- mapM (uncurry $ flip selectSudokus) [("Easy", easy),("Medium",medium),("Hard",hard),("Insane",insane)]
  time <- lift $ UI.h1 # set text "Time: "
  select <- lift $ UI.div #. "selectSudokus" #+ map return sudokus
  content <- lift $ UI.div #. "sudoku" #+ sudokuGrid [createCell row col sdkToDisplay | row <- [0..8], col <- [0..8]] 

  liftIO $ Reactive.Threepenny.register (UI.tick mytimer) $ const $ runUI mywindow $ showTime (return time) mystartTime
  -- (Re)draws the whole html body

  -- (listBox, caption) <- lift mysetup'
  -- listBox' <- element listBox
  -- caption' <- element caption

  -- testC <- testCellBeh
  -- (mycell, eMycell) <- lift $ createCell' 1 2 testC

  
  
  lift $ getBody mywindow # set children [caption, content, select, time]
  return ()
  where
    showTime time mystartTime = do
      now <- liftIO getCurrentTime
      time # set text (show $ diffUTCTime now mystartTime)
      return ()
    
selectSudokus :: SudokuLens -> String -> StateT App UI Element
selectSudokus lens caption = do
  app <- S.get
  sdks <- use lens
  let options = map (\(Sudoku sid _ _) -> UI.option # set value (show sid) # set text (show sid)) sdks

  select <- lift $ UI.select #. "selectSudoku" #+ options

  -- Show the selected sudoku on selection change
  lift $ on UI.selectionChange select  $ \(Just val) -> do
    let sudokuWithId :: Integer -> SudokuWithId
        sudokuWithId searchId = head $ filter (\(Sudoku sid _ _) -> sid == searchId) sdks 
        getIdOfNthSudoku nth = let (Sudoku sid _ _) = (sdks!!nth) in sid
        -- This is the sudoku selected in the select box
        selectedSudoku = sudokuWithId $ getIdOfNthSudoku val
        -- The selected sudoku should be the first in the select box
        sudokusOfBox = selectedSudoku : filter (selectedSudoku /=) sdks
    now <- liftIO getCurrentTime

    
    evalStateT

      (do
       lens .= sudokusOfBox
       startTime .= now
        -- redraw the body
       showSudoku)
      -- Update the state with the selectedSudoku
      (app & chosen .~ selectedSudoku)

  -- Create the select box with a caption
  lift $ UI.div #+ [UI.h1 # set text caption, element select]

-- Separates the cells into 9 sudoku blocks and concatenates them
sudokuGrid :: [UI Element] -> [UI Element]
sudokuGrid cells =  concatMap (concat . blockify [[],[],[]]) $ chunksOf 27 cells
  where
    blockify :: [[UI Element]] -> [UI Element] -> [[UI Element]]
    blockify blocks [] = [ [UI.div #. "square" #+ cs] | cs <- blocks]
    blockify acc cs = blockify (zipWith (++) acc row ) restCells
      where
      row = chunksOf 3 $ take 9 cs
      restCells = drop 9 cs

-- taken from Network-CGI-Protocol package
maybeRead = fmap fst . listToMaybe . reads

createCell :: Int -> Int -> SudokuWithId -> UI Element
createCell r c (Sudoku _ _ digits) =
  let row = show r
      col = show c
      sudoku = chunksOf 9 digits 
      -- these classes are not necessary but good to indicate that the
      -- cell is in its correct place in the grid
      classes = "cell row"++row++"col"++col
      solution = case (sudoku!!r)!!c of
        Free digit -> digit
        Guess digit -> digit
      in

   do
    cell <- UI.div #. classes
    cellInput <- UI.input # set (attr "maxlength") "1"

    -- Change background of a digit square on value change
    on UI.valueChange cellInput $ \val -> do
      if val == "" then
        return cell #. classes
        else if read val == solution then
               return cell #. (classes ++ " right")
             else
               return cell #. (classes ++ " wrong")
      return ()

    -- Mark the free digits 
    case (sudoku!!r)!!c of
      Free _ -> element cellInput # set value (show solution) # set (attr "readonly") "true" #. "free"
      _ -> element cellInput # set value ""

    element cell #+ [return cellInput]
