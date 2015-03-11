{-# LANGUAGE CPP, PackageImports, RecursiveDo #-}

module Main where

#ifdef CABAL
import qualified "threepenny-gui" Graphics.UI.Threepenny      as UI
import           "threepenny-gui" Graphics.UI.Threepenny.Core
#else
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
#endif
import           Paths
import           Reactive.Threepenny

import           Common
import           Control.Arrow               (first, second, (&&&), (***),
                                              (+++), (>>>))
import           Control.Monad               (forM, join, liftM, sequence, void,
                                              when)
import           Data.Array.IArray
import           Data.Char                   (isDigit)
import           Data.List                   (foldl1', groupBy, isPrefixOf,
                                              sort)
import           Data.List.Split
import           Data.Maybe                  (fromJust, fromMaybe, isJust,
                                              isNothing, listToMaybe)
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           System.FilePath             ((<.>), (</>))
import           System.IO.Unsafe            (unsafePerformIO)


main :: IO ()
main = do
    static <- getStaticDir
    let database = static </> db -- database file is in /wwwroot
    sudokus <- readSudokus database :: IO [[SudokuWithId]]

    startGUI defaultConfig
        { tpPort       =  Just 10000
        , tpStatic     = Just static
        } (runApp $ zip ["Easy", "Medium", "Hard", "Insane"] sudokus)

runApp :: LabeledSudokus -> Window -> UI ()
runApp labeledSudokus w = void $ mdo
  UI.addStyleSheet w "sudoku.css"
  let (lvs, sudokus) = unzip labeledSudokus

  ---------------------------------------------------------------
  ---- Create Select Boxes and their selection event streams ----
  ---------------------------------------------------------------
  listBoxesAndEvents <- createSelectBoxes sudokus :: UI [(UI.ListBox String, Event SudokuId)]
  let (listBoxes, eListBoxes) = unzip listBoxesAndEvents :: ([UI.ListBox String], [Event SudokuId])

   -- Event of selecting any sudoku item of any sudoku level select box
      eSelectSudoku = filterSudokusE $ head <$> unions eListBoxes 

  bDisplayedSudoku <- stepper "0" eSelectSudoku :: UI (Behavior SudokuId)

  let sudokuIsDifferentFromCurrent = (/=) <$> bDisplayedSudoku :: Behavior (SudokuId -> Bool)
   -- event of selecting a NEW sudoku item of any sudoku level select box
      eSelectNewSudoku = filterApply sudokuIsDifferentFromCurrent eSelectSudoku :: Event SudokuId 

  -- on startup no sudoku is shown so it is Nothing when the application starts
  bCurrentSudoku <- (stepper Nothing $ Just <$> eSelectNewSudoku) :: UI (Behavior (Maybe SudokuId))
  ---------------------------------------------------------------

  ------------------------------------------------------------
  ---- Create cells and corresponding error event streams ----
  ------------------------------------------------------------
  cells <- sequence [createCell col row' eSelectNewSudoku eNewDigitAtColRow | row' <- [0..8], col <- [0..8],
                     let eNewDigitAtColRow = eNewDigit sudokus eSelectNewSudoku (col, row')]
  let (cellElems, eCorrect) = unzip cells :: ([Element], [Event Correctness])
  ------------------------------------------------------------

  ---------------------------
  -- Create content panels --
  ---------------------------
  let bChooseStyle = maybe "welcome" (const "welcome hide") <$> bCurrentSudoku
      bSudokuGridStyle = maybe "sudoku hide" (const "sudoku") <$> bCurrentSudoku
  choosePanel <- UI.div #+ [UI.h2 # set text "Choose a Sudoku to solve:"
                                  # set UI.style [("font-style","italic")]]
                        # sink UI.class_ bChooseStyle

  contentPanel <- UI.div #. "sudoku hide"
                         # sink UI.class_ bSudokuGridStyle
                         #+ sudokuGrid cellElems
  ---------------------------

  --------------------------------------
  -- Create wrappers for select boxes --
  --------------------------------------
  -- make sure the wrapper for the level of the current sudoku gets highlighted
  selectWrappers <- forM  (zip listBoxes [0..numberOfLevels-1]) $ \(listBox,nr) -> do
             let otherEvents = take nr eListBoxes ++ drop (nr+1) eListBoxes :: [Event SudokuId]
                 ownEvent = eListBoxes!!nr :: Event SudokuId
                 containerPanel = UI.div
             level <- UI.h2 # set text (lvs !! nr)
             -- if a sudoku of other select box gets selected then the class is "selectSudokus"
             -- if a sudoku of own select box gets selected then the class is "selectSudokus hightlighted"
             bWrapperClass <- stepper "selectSudokus" $ head <$>
                              unions ["selectSudokus" <$ filterSudokusE (head <$> unions otherEvents),
                                      "selectSudokus highlighted" <$ filterSudokusE ownEvent]
             containerPanel # sink UI.class_ bWrapperClass
                            #+ [widget level, element listBox]
  --------------------------------------

  --------------------
  -- Sudoku Caption --
  --------------------
  captionPanel <- UI.h1
  let bCaption = maybe "Welcome to Sudoku Land!" ("Sudoku "++) <$> bCurrentSudoku
  widget captionPanel # sink text bCaption
  --------------------

  ----------------------------
  --- Count correct number ---
  ----------------------------
  bCorrectnessOfInput <- forM eCorrect
                         (stepper (0::Int) . fmap (\x -> if x == Correct then 1 else 0)) :: UI [Behavior Int]
  let bSudokuCorrect = foldl1' (\x y -> (+) <$> x <*> y) bCorrectnessOfInput
      sudokuSize = 81 
  ----------------------------
-- 28

  -----------------
  -- Error Panel --
  -----------------
  errorPanel <- UI.h3

  let eIncorrectDigit = map (\x -> () <$ filterE (== NotCorrect) x) eCorrect :: [Event ()]
  bErrorInt <- accumB 0 $ concatenate <$> unions [
    const 0 <$ eSelectNewSudoku,
    (+1)  <$ unions eIncorrectDigit
    ] :: UI (Behavior Int)

  let bError = (show &&& (\x -> if x == 1 then " Error" else " Errors") >>> uncurry (++)) <$> bErrorInt
  widget errorPanel # sink text bError # sink UI.class_ (maybe "hide" (const "") <$> bCurrentSudoku)
  -----------------

  -----------------
  -- Timer Panel --
  -----------------
  timePanel <- UI.h3
  timer <- UI.timer # set UI.interval 1000 # sink UI.running ( (< sudokuSize) <$> bSudokuCorrect)

  eTimerInt <- accumE (0 :: Int) $ concatenate <$> unions [
    const 0 <$ eSelectNewSudoku,
    (+1) <$ UI.tick timer
    ] :: UI (Event Int)

  let padNumber x = if x < 10 then '0':show x else show x
      eTimerFormatted = ((\(x,y) -> "Time: " ++ x ++ ":" ++ y) . join (***) padNumber . (`quotRem` 60)) <$> eTimerInt :: Event String

  bTimer <- stepper "" eTimerFormatted
  widget timePanel # sink text bTimer # sink UI.class_ (maybe "hide" (const "") <$> bCurrentSudoku)
  -----------------

  -----------------------
  --- "Solved!" Panel ---
  -----------------------
  solvedPanel <- UI.h3 # set UI.class_ "solved" # sink text bSolved
  let bSolved = (\x -> if x >= sudokuSize then "Solved!" else "") <$> bSudokuCorrect

  sidebarPanel <- UI.div  # set UI.class_ "sidebar" # set children (selectWrappers ++ [errorPanel, timePanel, solvedPanel])
  -----------------------

  -- Add panels to body
  getBody w # set children [captionPanel, choosePanel, contentPanel, sidebarPanel]



createSelectBoxes :: [[SudokuWithId]] -> UI [(UI.ListBox String, Event String)]
createSelectBoxes sudokus'' = mdo
  when (length sudokus'' /= numberOfLevels) $
    fail "\nExactly 4 levels of difficulty are expected!\n"

  let lvs = [0..numberOfLevels-1]
      sudokus = map (sudokus''!!) lvs :: [[SudokuWithId]]

  listBoxes <- mapM (\(x,y,z) -> UI.listBox x y z) $ zipLength4 bItems bSelectedItems bDisplayItems

  let bDisplayItems = replicate numberOfLevels (pure UI.string) :: [Behavior (String -> UI Element)]
      eSelections = map (rumors . UI.userSelection) listBoxes :: [Event (Maybe String)]

  -- bSelectedItems designates what item of the select box is selected at any time
  bSelectedItems <- forM lvs (\x -> do
                   let eventsOfOtherLevels = take x eSelections ++ drop (x+1) eSelections
                   stepper (Just chooseSudokus) $ head <$> unions [
                    -- Allow the user to select the item
                       eSelections !! x, 
                    -- When the user selects a sudoku item of another select box then select the item chooseSudokus
                       Just chooseSudokus <$ filterE (notElem chooseSudokus)
                                             (unions $ map filterJust eventsOfOtherLevels)]) :: UI [Behavior (Maybe SudokuId)]

  let sudokusIds = map (map $ \(Sudoku sid _ _) -> show sid) sudokus :: [[SudokuId]]
      allOptions = map (chooseSudokus : ) sudokusIds
      allOptionsByLevel = zip lvs allOptions :: [(Int, [String])]

  -- bItems designates what items the select box has as its options at any time
  bItems <- forM allOptionsByLevel $ \(l,opt) -> do
              let eventsOfOtherLevels = take l eSelections ++ drop (l+1) eSelections
              stepper opt $ head <$> unions [

                -- Do not show chooseSudokus when a sudoku of the own selectBox is displayed
                sudokusIds !! l <$ filterSudokusE (filterJust (eSelections !! l)), 

                -- Show chooseSudokus when a sudoku of another select box gets currently selected
                allOptions !! l <$ filterE (notElem chooseSudokus) (unions $ map filterJust eventsOfOtherLevels)] 

  -- return the listboxes coupled with the event stream of the select box option selection
  return $ zip listBoxes $ map filterJust eSelections


chooseSudokus = "--- choose a Sudoku ---"
filterSudokusE = filterE (/= chooseSudokus)

-- Gets the event stream of the specific digit at (col,row) whenever a new sudoku is selected
eNewDigit :: [[SudokuWithId]] -> Event SudokuId -> (Int, Int) -> Event (Maybe Digit)
eNewDigit sudokus eSID (col, row) =
  let eSudokuWithId :: Event (Maybe SudokuWithId)
      eSudokuWithId = findSudoku' (concat sudokus) . read <$> eSID
     in
  fmap ((!!col) . (!!row) . chunksOf 9 . extractDigits ) <$> eSudokuWithId

createCell :: Int -> Int -> Event SudokuId -> Event (Maybe Digit) -> UI (Element, Event Correctness)
createCell x y eSelectNewSudoku eNewDigit = do
     let classes = "cell row"++show y++"col"++show x
         isFree digit' = case digit' of { (Guess _) -> False ; _ -> True}
         digitString digit' = case digit' of
          (Guess _) -> ""
          (Free a) -> show a
         correctOrNot False = NotCorrect
         correctOrNot True  = Correct

     -- The digit of the cell is Nothing in the beginning
     digit <- stepper Nothing eNewDigit :: UI (Behavior (Maybe Digit))
     cell <- UI.div #. classes

     -- input element of the cell. it gets initialized with the behavior of only showing the digit if it s a (Free digit) and not a (Guess a)
     cellInput <- UI.entry (maybe "" digitString <$> digit)
     element cellInput # sink UI.enabled (maybe False (not . isFree) <$> digit) # set (attr "maxlength") "1"
     cell # set' children [getElement cellInput]


     -- bDigit is the context of the cell input
     let bDigit = UI.userText cellInput :: Tidings String
         eInput'' = rumors bDigit :: Event String

     eInput' <- stepper "0" $ head <$> unions [
       "0" <$ eNewDigit,
       eInput''
       ]
     -- eInput' is the event stream of all user inputs to this cell
     -- eInput is the event stream of all user inputs to this cell which are different than the previous one
     let isInputDifferentFromCurrent = (/=) <$> eInput' :: Behavior (String -> Bool)
         eInput = filterApply isInputDifferentFromCurrent eInput''

         isDigit' digit' = if length digit' /= 1 then False else isDigit $ head digit'

         eNonDigitInput = filterE (not . isDigit') eInput :: Event String

         isCorrectAnswer :: Maybe Digit -> Maybe Int -> Correctness
         isCorrectAnswer correctDigit userDigit = case correctDigit of
                      Nothing -> NotCorrect
                      Just digit -> case digit of
                                      Guess digit' -> maybe NotADigit (correctOrNot . (== digit')) userDigit
                                      Free _ -> Correct
         eCorrectnessOfInput = isCorrectAnswer <$> digit <@> (maybeRead <$> eInput) :: Event Correctness

         eCorrect = () <$ filterE (== Correct) eCorrectnessOfInput
         eNotCorrect = () <$ filterE (== NotCorrect) eCorrectnessOfInput

         eInputClasses = head <$> unions [
           classes ++ " wrong" <$ eNotCorrect, -- add class 'wrong' when an incorrect digit is the guess
           classes ++ " right" <$ eCorrect, -- add class 'right' when the correct digit is guessed
           classes <$ eNonDigitInput -- only show default classes when the input is not a single digit
           ] :: Event String

     bInputClasses <- stepper classes $ head <$> unions [
       classes <$ eSelectNewSudoku, -- when a new sudoku gets selected remove the wrong/right classes
       eInputClasses
       ]
     widget cell # sink UI.class_ bInputClasses


     let eCorrectness = head <$> unions [
           maybe NotCorrect (correctOrNot . isFree) <$> eNewDigit,
           eCorrectnessOfInput
           ]

     return (cell, eCorrectness)


--- taken from Network-CGI-Protocol package
maybeRead :: String -> Maybe Int
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

-- This is a specialisation of the normal zip function which works for lists of length 4
-- It is necessary as the value recursion of createSelectBoxes does not  work otherwise
zipLength4 ~[a,b,c,d] ~[e,f,g,h] ~[i,j,k,l] = [(a,e,i),(b,f,j),(c,g,k),(d,h,l)]

findSudoku :: [SudokuWithId] -> Integer -> SudokuWithId
findSudoku sudokus sid = head $ filter (\(Sudoku sid' _ _ ) -> sid' == sid) sudokus

findSudoku' :: [SudokuWithId] -> Integer -> Maybe SudokuWithId
findSudoku' sudokus sid = case filter (\(Sudoku sid' _ _ ) -> sid' == sid) sudokus of
  [] -> Nothing
  (a:_) -> Just a

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
