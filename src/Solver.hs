module Solver  where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as BV (generate,(!))
import Data.List (foldl',sort,group)
import Data.Char (chr, ord)
import Data.Word
import Data.Bits
import Control.Monad
import Data.Maybe

-- Types
type Alphabet   = Word8
type Hypothesis = Word32

-- Hypotheses space is a matrix of independed hypoteses
type HypothesesSpace    = V.Vector Hypothesis

-- Set up spatial transformers / discriminators to reflect the spatial
-- properties of a Sudoku puzzle
ncells = 81

-- vector rearrangement functions
rows        :: HypothesesSpace -> HypothesesSpace
rows        = id

columns     :: HypothesesSpace -> HypothesesSpace
columns vec = V.map (\cidx -> vec `V.unsafeIndex` cidx) cIndices
            where cIndices = V.fromList [r*9 + c | c <-[0..8], r<-[0..8]]

subGrids    :: HypothesesSpace -> HypothesesSpace
subGrids vec= V.map (\idx -> vec `V.unsafeIndex` idx) sgIndices
            where sgIndices = V.fromList [i + bc + br | br <- [0,27,54], bc <- [0,3,6], i<-[0,1,2,9,10,11,18,19,20]]

-- needs to be boxed, because vector elements are not primitives
peersDiscriminators = BV.generate ncells discriminator
        where
            discriminator idx1 = V.zipWith3 (\r c s -> (r || c || s)) rDscr cDscr sDscr
                where
                    rDscr = V.generate ncells (\idx2 -> idx1 `div` 9 == idx2 `div` 9)
                    cDscr = V.generate ncells (\idx2 -> idx1 `mod` 9 == idx2 `mod` 9)
                    sDscr = V.generate ncells (\idx2 -> subGridOfidx1 == subGrid idx2)
                        where
                            subGridOfidx1 = subGrid idx1
                            subGrid idx = (idx `div` 27, (idx `div` 3) `mod` 3)

-- Let's implement the logic

-- Level 0 logic (enforce consistency):
-- We can't have multiple same solutions in a peer unit,
-- eliminate solutions from other hypotheses
enforceConsistency :: HypothesesSpace -> Maybe HypothesesSpace
enforceConsistency hypS0 = 
        V.foldM solutionReduce hypS0 $ V.findIndices newSingle hypS0

solutionReduce :: HypothesesSpace -> Int -> Maybe HypothesesSpace
solutionReduce hypS0 idx =
        let sol     = hypS0 V.! idx
            peers   = peersDiscriminators BV.! idx
            hypS1   = V.zipWith reduceInUnit peers hypS0
                where
                    reduceInUnit p h
                                | p && (h == sol)   = setSolution sol
                                | p                 = h `minus` sol
                                | otherwise         = h
        in if V.any empty hypS1
            then return hypS1
            else if V.any newSingle hypS1
                    then enforceConsistency hypS1 -- constraint propagation
                    else return hypS1

-- Level 1 logic is rather simple:
-- We tally up all unknown values in a given unit,
-- if a value occurs only once, then it must be the solution!
localizeSingles :: HypothesesSpace -> Maybe HypothesesSpace
localizeSingles unit = let known = maskChoices $ accumTally $ V.filter single unit
        in if dups known
            then Nothing
            else
                case (filterSingles $ accumTally $ V.filter (not . single) unit) `minus` known of
                    0       -> return unit
                    sl      -> return $ replaceWith unit sl
                        where
                            replaceWith :: V.Vector Hypothesis -> Hypothesis -> V.Vector Hypothesis
                            replaceWith unit s = V.map (\u -> if 0 /= maskChoices (s .&. u) then s `Solver.intersect` u else u) unit

-- Level 2 logic is a bit more complicated:
-- Say in a given unit, we find exactly two places with the hypothesis {1,9}.
-- Then obviously, the value 1 and 9 can only occur in those two places.
-- All other ocurrances of the value 1 and 9 can eliminated.
localizePairs :: HypothesesSpace -> Maybe HypothesesSpace
localizePairs unit = let pairs = V.toList $ V.filter pair unit
        in if nodups pairs
            then return unit
            else
                case map head $ filter lpair $ tally pairs of
                    []          ->  return unit
                    pl@(p:ps)   ->  return $ foldl' eliminateFrom unit pl
                        where -- "subtract" pair out of a hypothesis
                            eliminateFrom :: V.Vector Hypothesis -> Hypothesis -> V.Vector Hypothesis
                            eliminateFrom unit p = V.map (\u -> if u /= p then u `minus` p else u) unit

-- Level 3 logic resembles the level 2 logic:
-- If we find exactly three places with the hypothesis {1,7,8} in a given unit, then all other ...
-- you'll get the gist!
localizeTriples :: HypothesesSpace -> Maybe HypothesesSpace
localizeTriples unit = let triples = V.toList $ V.filter triple unit
        in if nodups triples
            then return unit
            else
                case map head $ filter ltriple $ tally triples of
                    []          ->  return unit
                    tl@(t:ts)   ->  return $ foldl' eliminateFrom unit tl
                        where -- "subtract" triple out of a hypothesis
                            eliminateFrom :: V.Vector Hypothesis -> Hypothesis -> V.Vector Hypothesis
                            eliminateFrom unit t = V.map (\u -> if u /= t then u `minus` t else u) unit

-- Even higher order logic is easy to implement, but becomes rather useless in the general case!

-- Implement the whole nine yard: constraint propagation and search

applySameDimensionLogic :: HypothesesSpace -> Maybe HypothesesSpace
applySameDimensionLogic hyp0 = do
        res1 <- logicInDimensionBy rows chainedLogic hyp0
        res2 <- logicInDimensionBy columns chainedLogic res1
        logicInDimensionBy subGrids chainedLogic res2
            where
                chainedLogic = localizeSingles >=> localizePairs >=> localizeTriples

logicInDimensionBy :: (HypothesesSpace -> HypothesesSpace) -> (HypothesesSpace -> Maybe HypothesesSpace) -> HypothesesSpace -> Maybe HypothesesSpace
logicInDimensionBy trafo logic hyp = liftM (trafo . V.concat) $ mapM (\ridx -> do logic $ V.unsafeSlice ridx 9 hyp') [r*9 | r<- [0..8]]
        where
            hyp' :: HypothesesSpace
            hyp' = trafo hyp

prune :: HypothesesSpace -> Maybe HypothesesSpace
prune hypS0 = do
        hypS1 <- applySameDimensionLogic =<< enforceConsistency hypS0
        if V.any newSingle hypS1
            then prune hypS1    -- effectively implemented constraint propagation
            else do
                hypS2 <- applySameDimensionLogic hypS1
                if hypS1 /= hypS2
                    then prune hypS2    -- effectively implemented a fix point method
                    else return hypS2

search :: HypothesesSpace -> Maybe HypothesesSpace
search hypS0
        | complete hypS0    = return hypS0
        | otherwise         = do msum [prune hypS1 >>= search | hypS1 <- expandFirst hypS0]

-- guessing order makes a big difference!!
expandFirst :: HypothesesSpace -> [HypothesesSpace]
expandFirst hypS
        | suitable == []    = []
        | otherwise         = let (_, idx) = minimum suitable -- minimum is the preferred strategy!
                              in map (\choice -> hypS V.// [(idx, choice)]) (split $ hypS V.! idx)
    where
        suitable = filter ((>1) . fst) $ V.toList $ V.imap (\idx e -> (numChoices e, idx)) hypS

-- Some very useful tools:
-- partition a list into sublists
chop            :: Int -> [a] -> [[a]]
chop n []       =  []
chop n xs       =  take n xs : chop n (drop n xs)

-- when does a list have no duplicates
nodups          :: Eq a => [a] -> Bool
nodups []       =  True
nodups (x:xs)   =  not (elem x xs) && nodups xs

dups            :: Hypothesis -> Bool
dups t          = (filterDups t) /= 0

tally           :: Ord a => [a] -> [[a]]
tally           = group . sort

empty           :: Hypothesis -> Bool
empty n         = (maskChoices n) == 0

single          :: Hypothesis -> Bool
single n        = (numChoices n) == 1

lsingle         :: [a] -> Bool
lsingle [n]     = True
lsingle _       = False

pair            :: Hypothesis -> Bool
pair n          = numChoices n == 2

lpair           :: [a] -> Bool
lpair (x:xs)    = lsingle xs
lpair _         = False

triple          :: Hypothesis -> Bool
triple n        = (numChoices n) == 3

ltriple         :: [a] -> Bool
ltriple (x:xs)  = lpair xs
ltriple _       = False

complete        :: HypothesesSpace -> Bool
complete        = V.all single

-- The bit gymnastics (wish some were implemented in Data.Bits)
-- bits 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 .. 27 28 29 30 31 represents
--      h - h - h - h - h - h  -  h  -  h  -  h  -  .. s  l  l  l  l
-- with
--      h : 1 iff element is part of the hypothesis set
--      l : 4 bits for the cached number of h bits set
--      s : 1 iff a single solution for the cell is found

-- experiment with different strategies
split           :: Hypothesis -> [Hypothesis]
split 0         = []
split n         = [n `minus` bit1, (bit 28) .|. bit1]
    where bit1 = (bit $ firstBit n)

minus           :: Hypothesis -> Hypothesis -> Hypothesis
xs `minus` ys
        | maskChoices (xs .&. ys) == 0  = xs
        | otherwise = zs .|. ((countBits zs) `shiftL` 28)
                        where zs = maskChoices $ xs .&. (complement ys)

numChoices      :: Hypothesis -> Word32
numChoices n    = (n `shiftR` 28) 

newSingle       :: Hypothesis -> Bool
newSingle n     = (n `shiftR` 27) == 2

isSolution      :: Hypothesis -> Bool
isSolution n    = n `testBit` 27

setSolution     :: Hypothesis -> Hypothesis
setSolution n   = n `setBit` 27

maskChoices     :: Hypothesis -> Hypothesis
maskChoices n   =  n .&. 0x07FFFFFF

intersect       :: Hypothesis -> Hypothesis -> Hypothesis
intersect x y   = z .|. ((countBits z) `shiftL` 28)
                    where z = maskChoices $ x .&. y

countBits       :: Word32 -> Word32 -- would be wonderful if Data.Bits had such a function
countBits 0     = 0
countBits n     = (cBLH 16 0xFFFF . cBLH 8 0xFF00FF . cBLH 4 0x0F0F0F0F . cBLH 2 0x33333333 . cBLH 1 0x55555555) n
cBLH            :: Int -> Word32 -> Word32 -> Word32
cBLH s mask n   = (n .&. mask) + (n `shiftR` s) .&. mask

firstBit        :: Hypothesis -> Int -- should also be in Data.Bits
firstBit 0      = 0 -- stop recursion !!
firstBit n
        | n .&. 1 > 0       = 0
        | otherwise         = (+) 1 $ firstBit $ n `shiftR` 1

accumTally      :: V.Vector Hypothesis -> Hypothesis
accumTally nl   = V.foldl' accumTally2 0 nl
accumTally2     :: Word32 -> Word32 -> Word32
accumTally2 t n = (+) t $ n .&. (((complement t) .&. 0x02AAAAAA) `shiftR` 1)

filterSingles   :: Hypothesis -> Hypothesis
filterSingles t = t .&. (((complement t) .&. 0x02AAAAAA) `shiftR` 1)

filterDups      :: Hypothesis -> Hypothesis
filterDups t    = (t .&. 0x02AAAAAA) `shiftR` 1

defaultHypothesis :: Hypothesis
defaultHypothesis = 0x90015555 -- all nine alphabet elements are set

mapAlphabet :: V.Vector Hypothesis
mapAlphabet = V.replicate 256 defaultHypothesis V.// validDigits
    where
        validDigits :: [(Int, Hypothesis)]
        validDigits = [(ord i, (bit 28) .|. (bit $ 2*(ord i - 49))) | i <- "123456789"]

toChar :: Hypothesis -> [Char]
toChar s
        | single s      = [normalize s]
        | otherwise     = "."
        where
            normalize s = chr $ (+) 49 $ (firstBit s) `shiftR` 1

toCharDebug :: Hypothesis -> [Char]
toCharDebug s
        | isSolution s          = ['!', normalize s]
        | single s              = [normalize s]
        | otherwise             = "{" ++ digits ++ "}"
        where
            normalize s = chr $ (+) 49 $ (firstBit s) `shiftR` 1
            digits = zipWith test "123456789" $ iterate (\e -> e `shiftR` 2) s
            test c e
                    | e.&.1 == 1    = c
                    | otherwise     = '.'

-- Initial hypothesis space 
initialize :: String -> Maybe HypothesesSpace
initialize g =  if all (`elem` "0.-123456789") g
                then
                    let
                        hints = zip [0..] translated
                        translated = map  (\c -> mapAlphabet V.! ord c) $ take ncells g
                    in Just $ (V.replicate ncells defaultHypothesis) V.// hints
                else Nothing

-- Display (partial) solution
printResultD :: HypothesesSpace -> IO ()
printResultD = putStrLn . toString
    where
        toString :: HypothesesSpace -> String
        toString hyp = unlines $ map translate . chop 9 $ V.toList hyp
            where
                translate = concatMap (\s -> toCharDebug s ++ " ")

printResult :: HypothesesSpace -> IO ()
printResult = putStrLn . toString
    where
        toString :: HypothesesSpace -> String
        toString hyp = translate (V.toList hyp)
            where
                translate = concatMap (\s -> toChar s ++ "")

printSolution :: HypothesesSpace -> String
printSolution = toString
    where
        toString :: HypothesesSpace -> String
        toString hyp = translate (V.toList hyp)
            where
                translate = concatMap (\s -> toChar s ++ "")

-- The entire solution process!
solve :: String -> Maybe HypothesesSpace
solve str = initialize str >>= prune >>= search

solveSudoku :: String -> String
solveSudoku str = case solve str of
  (Just solution) -> printSolution solution
  _ -> error "No Solution"
