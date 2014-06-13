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

import Data.List(foldl1', groupBy,sort,isPrefixOf)
import Data.Array.IArray
import Data.List.Split
import Data.Char(isDigit)
import Common
import Data.Maybe (listToMaybe, isNothing, isJust, fromMaybe, fromJust)
import System.FilePath((</>),(<.>))
import Control.Monad(when,join,void,liftM,sequence,forM)
import Control.Monad.Trans.Class
import Data.Time
import Control.Arrow(first,second,(***),(+++),(&&&),(>>>))
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

    startGUI defaultConfig
        { tpPort       =  10000
        , tpStatic     = Just static
        } (runApp $ zip ["Easy", "Medium", "Hard", "Insane"] sudokus)

fromSQLRow [sqlId, sqlLevel, sqlContent] =
  let sid = fromSql sqlId
      level = read $ fromSql sqlLevel 
      content = read $ fromSql sqlContent  in
  Sudoku sid level content

readSudokus :: FilePath -> IO [[SudokuWithId]]
readSudokus database  = do
  conn <- connectSqlite3 database
  rows <- quickQuery' conn "SELECT * FROM sudokus" []
  Database.HDBC.disconnect conn
  let sudokus = groupBy (\(Sudoku _ l1 _) (Sudoku _ l2 _) -> l1 == l2) $ sort $ map fromSQLRow rows
  return sudokus

  
createSelectBoxes :: [[SudokuWithId]] -> UI [(UI.ListBox String, Event String)]
createSelectBoxes sudokus'' = mdo
  when (length sudokus'' /= numberOfLevels) $
    fail "\nExactly 4 levels of difficulty are expected!\n"

  let lvs = [0..numberOfLevels-1]
      sudokus = map (sudokus''!!) lvs
  listBoxes <- mapM (\(x,y,z) -> UI.listBox x y z) $ zipLength4 bItems bSelections  bDisplayItems
  let bDisplayItems = replicate numberOfLevels $ pure UI.string
      eSelections = map (rumors . UI.userSelection) listBoxes

  -- bSelections designates what item of the select box is selected at any time
  bSelections <- forM lvs $ \x -> stepper (Just chooseSudokus) $ head <$> unions [
                   eSelections !! x, -- allow the user to select the item
                   Just chooseSudokus <$ filterE (notElem chooseSudokus) (unions $ map filterJust $ take x eSelections ++ drop (x+1) eSelections) -- when the user selects a sudoku item of another select box then select the item chooseSudokus
                   ]
  let sudokusIds = map (map $ \(Sudoku sid _ _) -> show sid) sudokus
      allOptions = map (chooseSudokus : ) sudokusIds

  -- bItems designates what items the select box has as its options at any time
  bItems <- forM (zip lvs allOptions) $ \(l,opt) -> stepper opt $ head <$> unions [
              sudokusIds !! l <$ filterSudokusE (filterJust (eSelections !! l)), -- Do not show chooseSudokus when a sudoku of the own selectBox is displayed
              allOptions !! l <$ filterE (notElem chooseSudokus) (unions $ map filterJust $ take l eSelections ++ drop (l+1) eSelections)] -- Show chooseSudokus when a sudoku of another select box gets currently selected

  -- return the listboxes coupled with the event stream of the select box option selection
  return $ zip listBoxes $ map filterJust eSelections


-- This is a specialisation of the normal zip function which works for lists of length 4
-- It is necessary as the value recursion of createSelectBoxes does not  work otherwise
zipLength4 ~[a,b,c,d] ~[e,f,g,h] ~[i,j,k,l] = [(a,e,i),(b,f,j),(c,g,k),(d,h,l)]

findSudoku :: [SudokuWithId] -> Integer -> SudokuWithId
findSudoku sudokus sid = head $ filter (\(Sudoku sid' _ _ ) -> sid' == sid) sudokus 

findSudoku' :: [SudokuWithId] -> Integer -> Maybe SudokuWithId
findSudoku' sudokus sid = case filter (\(Sudoku sid' _ _ ) -> sid' == sid) sudokus of
  [] -> Nothing
  (a:as) -> Just a

chooseSudokus = "--- choose a Sudoku ---"
filterSudokusE = filterE (/= chooseSudokus) 

eNewDigit :: [[SudokuWithId]] -> Event String -> (Int, Int) -> Event (Maybe Digit)
eNewDigit sudokus eSID (col, row) = 
  let eSudokuWithId = findSudoku' (concat sudokus) . read <$> eSID
  in
  fmap ((!!col) . (!!row) . chunksOf 9 . extractDigits ) `fmap` eSudokuWithId

runApp :: LabeledSudokus -> Window -> UI ()
runApp labeledSudokus w = void $ mdo
  UI.addStyleSheet w "sudoku.css"
  let (lvs, sudokus) = unzip labeledSudokus

  listBoxesAndEvents <- createSelectBoxes sudokus 
  let (listBoxes, eListBoxes) = unzip listBoxesAndEvents
      eSudoku' = filterSudokusE $ head <$> unions eListBoxes -- event of selecting any sudoku item of any sudoku level select box
  bSudoku' <- stepper "0" eSudoku'
  let bSudoku''= fmap not <$> (==) <$> bSudoku' :: Behavior (String -> Bool)
      eSudoku = filterApply bSudoku'' eSudoku' -- event of selecting a NEW sudoku item of any sudoku level select box
      
  bListBox <- (stepper Nothing $ Just <$> eSudoku) :: UI (Behavior (Maybe String))

  -- bSudoku designates what Sudoku (Just SudokuWithId) is displayed at any moment or if no sudoku has been selected yet (Nothing)
  let bSudoku = fmap (findSudoku (concat sudokus) . read) <$> bListBox :: Behavior (Maybe SudokuWithId)

  -- Create cells and corresponding mistake event streams
  cells <- sequence [createCell col row' eSudoku eNewDigit' | row' <- [0..8], col <- [0..8],
                     let eNewDigit' = eNewDigit sudokus eSudoku (col, row')] 
  let (cellElems, eDigitCorrect, eCorrect) = unzip3 cells :: ([Element], [Event Bool], [Event Bool])

  -- Create content panels
  let bChoose = (\x -> if isJust x then "welcome hide" else "welcome") <$> bListBox 
      bSudokuGrid = (\x -> if isJust x then "sudoku" else "sudoku hide") <$> bListBox
  choosePanel <- UI.div #. "welcome" #+ [UI.h2 # set text "Choose a Sudoku ->"]
  widget choosePanel # sink UI.class_ bChoose
  contentPanel <- UI.div
  widget contentPanel # sink UI.class_ bSudokuGrid #+ sudokuGrid cellElems
  
  -- create wrappers for select boxes
  -- make sure the wrapper for the level of the current sudoku gets highlighted
  levelCaptions <- forM lvs $ \label -> UI.h2 # set text label
  selects <- forM  (zip3 listBoxes levelCaptions [0..numberOfLevels-1]) $
             \(listBox,level,nr) -> do
             let otherEvents = take nr eListBoxes ++ drop (nr+1) eListBoxes
                 ownEvent = eListBoxes!!nr
                 containerPanel = UI.div
             -- if a sudoku of other select box gets selected then the class is "selectSudokus"
             -- if a sudoku of own select box gets selected then the class is "selectSudokus hightlighted"
             bWrapperClass <- stepper "selectSudokus" $ head <$>
                              unions ["selectSudokus" <$ filterSudokusE (head <$> unions otherEvents),
                                      "selectSudokus highlighted" <$ filterSudokusE ownEvent]
             containerPanel # sink UI.class_ bWrapperClass #+ [widget level, element listBox]

  -- sudoku caption
  captionPanel <- UI.h1
  let bCaption = maybe "Welcome to Sudoku Land!" ("Sudoku "++) <$> bListBox
  widget captionPanel # sink text bCaption

  let eIncorrectDigit = map (\x -> () <$ filterE not x) eDigitCorrect
  -- bCorrectnessOfInput <- forM eCorrect $ \x -> stepper 0 (head <$> unions [((\x -> if x then 1 else 0) <$> x)])
  -- bCorrectnessOfInput <- forM eCorrect $ stepper False 
  bCorrectnessOfInput <- forM eCorrect $ stepper (0::Int) . fmap (\x -> if x then 1 else 0)
  -- let bSudokuCorrect = foldl1' (\x y -> (&&) <$> x <*> y) bCorrectnessOfInput :: Behavior Bool
  let bSudokuCorrect = foldl1' (\x y -> (+) <$> x <*> y) bCorrectnessOfInput
      eSudokuCorrect = ((\x -> const (x>28)) <$> bSudokuCorrect) `filterApply` (head <$> unions eCorrect)
  
  -- error
  errorPanel <- UI.h3
  bErrorInt <- accumB 0 $ concatenate <$> unions [
    const 0 <$ eSudoku,
    (+1)  <$ unions eIncorrectDigit
    ] :: UI (Behavior Int)
  -- let bError = (++ " Error") . show <$> bErrorInt
  let bError = (show &&& (\x -> if x == 1 then " Error" else " Errors") >>> uncurry (++)) <$> bErrorInt
  widget errorPanel # sink text bError
  widget errorPanel # sink UI.class_ (maybe "hide" (const "") <$> bListBox)

  -- timer
  timePanel <- UI.h3
  timer <- UI.timer # set UI.interval 1000

  eTimerInt <- accumE (0 :: Int) $ concatenate <$> unions [const 0 <$ eSudoku, (+1) <$ UI.tick timer] :: UI (Event Int)
  let padNumber x = if x < 10 then '0':show x else show x
      eTimerFormatted = ((\(x,y) -> x ++ ":" ++ y) . join (***) padNumber . (`quotRem` 60)) <$> eTimerInt :: Event String
  bTimer <- stepper "" eTimerFormatted
  widget timePanel # sink text bTimer
  widget timePanel # sink UI.class_ (maybe "hide" (const "") <$> bListBox)

  onEvent eSudokuCorrect $ const $ UI.stop timer
  onEvent eSudoku $ const $ UI.start timer

  -- Solved Panel
  solvedPanel <- UI.h3
  let bSolved = (\x -> if x>28 then "Gewonnen!" else "") <$> bSudokuCorrect 
  widget solvedPanel # sink text bSolved
  
  -- add panels to body
  getBody w # set children ([captionPanel, choosePanel, contentPanel] ++ selects ++ [errorPanel, timePanel, solvedPanel])


createCell :: Int -> Int -> Event String -> Event (Maybe Digit) -> UI (Element, Event Bool, Event Bool)
createCell x y eSudoku eNewDigit =
  let classes = "cell row"++show y++"col"++show x
      isFree digit' = case digit' of { (Guess _) -> False ; _ -> True}
      digitString digit' = case digit' of
        (Guess _) -> ""
        (Free a) -> show a 
      in
   do
     digit <- stepper Nothing eNewDigit
     cell <- UI.div #. classes
     
     -- input element of the cell. it gets initialized with the behavior of only showing the digit if it s a (Free digit) and not a (Guess a)
     cellInput <- UI.entry (maybe "" digitString <$> digit) 
     element cellInput # sink UI.enabled (maybe False (not . isFree) <$> digit) # set (attr "maxlength") "1"
     cell # set' children [getElement cellInput]
     let bDigit = UI.userText cellInput
         eInput'' = rumors bDigit

     

     eInput' <- stepper "0" $ head <$> unions [
       "0" <$ eNewDigit,
       eInput''
       ]
     -- eInput' is the event stream of all user inputs to this cell
     -- eInput is the event stream of all user inputs to this cell which are different than the previous one
     let bInput''= fmap not <$> (==) <$> eInput' :: Behavior (String -> Bool)
         eInput = filterApply bInput'' eInput''

         isDigit' digit' = if length digit' /= 1 then False else isDigit $ head digit'

         -- eDigitInput is event stream of all digit inputs (which are different than the previous one)
         eDigitInput = read <$> filterE isDigit' eInput :: Event Int
         eNonDigitInput = filterE (not . isDigit') eInput
         eCorrectnessOfInput = (\x y -> maybe False (either (\z -> maybe False (\a -> z == a) y) (const True) . toEither) x) <$> digit <@> (maybeRead <$> eInput) :: Event Bool
         eCorrectnessOfDigitInput = (\x y -> maybe False (either (==y) (const True) . toEither) x) <$> digit <@> eDigitInput :: Event Bool
         eCorrect = () <$ filterE id eCorrectnessOfDigitInput
         eNotCorrect = () <$ filterE not eCorrectnessOfDigitInput
         
         eInputClasses = head <$> unions [
           classes ++ " wrong" <$ eNotCorrect, -- add class 'wrong' when an incorrect digit is the guess
           classes ++ " right" <$ eCorrect, -- add class 'right' when the correct digit is guessed
           classes <$ eNonDigitInput -- only show default classes when the input is not a single digit
           ] 
     bInputClasses <- stepper classes $ head <$> unions [
       classes <$ eSudoku, -- when a new sudoku gets selected remove the wrong/right classes
       eInputClasses
       ]
     widget cell # sink UI.class_ bInputClasses

     let eCorrectness = head <$> unions [
           maybe False isFree <$> eNewDigit,
           eCorrectnessOfInput
           ]
     
     return (cell, eCorrectnessOfDigitInput, eCorrectness)


--- taken from Network-CGI-Protocol package
maybeRead = fmap fst . listToMaybe . reads
  
-- Separates the cells into 9 sudoku blocks and concatenates them
sudokuGrid :: [Element] -> [UI Element]
sudokuGrid cells =  concatMap (concat . blockify [[],[],[]]) $ chunksOf 27 $ map element cells
  where
    blockify :: [[UI Element]] -> [UI Element] -> [[UI Element]]
    blockify blocks [] = [ [UI.div #. "square" #+ cs] | cs <- blocks]
    blockify acc cs = blockify (zipWith (++) acc row ) restCells
      where
      row = chunksOf 3 $ take 9 cs
      restCells = drop 9 cs
