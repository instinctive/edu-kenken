-- prelude {{{2
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import BasePrelude

import Control.Monad.Trans.State
import Data.Map.Strict ( Map )
import Data.IntSet ( IntSet )
import Data.List.Split ( chunksOf )
import Data.Vector ( Vector, (!) )
import Lens.Micro.Platform
import System.IO
import qualified Data.Map.Strict as M
import qualified Data.IntSet as S
import qualified Data.Vector as V

-- from Algebra.Lattice
infixr 6 /\
(/\) = S.intersection

-- types {{{2

type Z = Int
type Eid = Char
type Loc = (Z,Z)
type Eqn = ([Loc],[[Z]])
type St = State Z
type Answer = (Z,Puzzle)

data Puzzle = Puzzle -- {{{2
    { _eqns :: Map Eid Eqn
    , _rows :: Vector IntSet
    , _cols :: Vector IntSet
    , _vals :: [(Loc,Z)]
    } deriving Show
p2x2 :: Puzzle -- {{{3
p2x2 = Puzzle eqns vals vals [] where
    eqns = M.fromList $ zip "ab"
        [ ([ (0,0),(0,1),(1,0) ],[ [1,1,2] ])
        , ([ (1,1)             ],[ [2]     ])
        ]
    vals = V.replicate 2 $ S.fromList [1,2]

p2x2' :: Puzzle -- {{{3
p2x2' = Puzzle eqns vals vals [] where
    eqns = M.fromList $ zip "ab"
        [ ([ (0,0),(0,1) ],[ [1,2] ])
        , ([ (1,0),(1,1) ],[ [1,2] ])
        ]
    vals = V.replicate 2 $ S.fromList [1,2]

count :: Puzzle -> [Answer] -- {{{2
count = flip evalState 0 . solve

solve :: Puzzle -> St [Answer] -- {{{2
solve p@Puzzle {..}
    | null cands = (:[]).(,p) <$> get
    | otherwise = concat <$> traverse (setValue p e x) (S.toList s)
  where
    (x,(e,s)) = minimumBy (comparing $ S.size.snd.snd) cands
    cands =
        [ (x,(e,s))
        | (e,(locs,cands)) <- M.toList _eqns
        , let evals = S.fromList (concat cands)
        , x@(r,c) <- locs
        , let s = _rows!r /\ _cols!c /\ evals
        ]

setValue :: Puzzle -> Eid -> Loc -> Z -> St [Answer] -- {{{2
setValue Puzzle {..} e x@(r,c) v = modify succ >> solve p
  where
    p = Puzzle
        { _eqns = _eqns & at e %~ (>>= f).fmap g
        , _rows = _rows & ix r %~ S.delete v
        , _cols = _cols & ix c %~ S.delete v
        , _vals = (x,v):_vals
        }
    f ([],_) = Nothing
    f eqn = Just eqn
    g = bimap (delete x) (map (delete v).filter (elem v))

getPuzzle :: IO Puzzle --- {{{2
getPuzzle = do
    r <- getLine
    let sz = length r
    rr <- replicateM (sz-1) getLine
    when (any ((/=sz).length) rr) $ error "inconsistent row length"
    let locs = [ (r,c) | r <- [0..sz-1], c <- [0..sz-1] ]
        (kno,unk) = partition (isDigit.snd) $ zip locs $ concat (r:rr)
        elocs = M.fromListWith (++) $ second (:[]).swap <$> unk
        emax = maximum (pred 'A':M.keys elocs)
    when (any (not.isAlpha.snd) unk) $ error "entries must be alphanumeric"
    ee <- replicateM (M.size elocs) $ readEqn . words <$> getLine
    let espec = M.fromList ee
    when (M.keys elocs /= M.keys espec) $ error "inconsistent equation ids"
    let ekno = zipWith mkKnown [succ emax..] kno
        eunk = zipWith (mkEqn sz) (M.toList elocs) (M.toList espec)
        eqns = M.fromList $ ekno ++ eunk
        vals = V.replicate sz $ S.fromList [1..sz]
    pure $ Puzzle eqns vals vals []
  where
    readEqn [e:_,o:_,s] = case readMaybe s of
        Nothing -> error $ "invalid equation value: " ++ show s
        Just v -> (e,(o,v))
    mkKnown e (x,d) = (e,([x],[[digitToInt d]]))
    mkEqn sz (e,locs) (_,(o,v))
        | null cands = error $ "invalid equation:" ++ show e
        | otherwise = (e,(locs,cands))
      where
        n = length locs
        cands :: [[Z]]
        cands = case o of
            '+' -> multi sum
            '*' -> multi product
            '-' -> two (+)
            '/' -> two (*)
            c -> error $ "invalid operator: " ++ show c
        multi f =
            filter ((==v).f) . filter nondec
            $ sequence $ replicate n [1..sz]
        nondec xx = and $ zipWith (<=) xx (tail xx)
        two (#) = if n /= 2 then [] else
            [ [a,b] | a <- [1..sz-1], b <- [a+1..sz], a # v == b ]

main :: IO () -- {{{2
main = getPuzzle >>= out . count where
    out [] = hPutStrLn stderr "IMPOSSIBLE"
    out ((n,Puzzle {..}):_) = do
        let sz = V.length _rows
            pct = 100 * fromIntegral (sz*sz) / fromIntegral n
        hPutStrLn stderr $ printf "%d decisions (%4.2f%% correct)\n" n (pct :: Double)
        let soln = chunksOf sz $ intToDigit.snd <$> sort _vals
        traverse_ putStrLn soln

