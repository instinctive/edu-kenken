-- prelude {{{1
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

data Puzzle = Puzzle -- {{{1
    { _eqns :: Map Eid Eqn
    , _rows :: Vector IntSet
    , _cols :: Vector IntSet
    , _vals :: [(Loc,Z)]
    } deriving Show

-- solution {{{1
data Next = Fail | Done | Next Eid Loc IntSet
    deriving ( Eq, Show )

solve :: Puzzle -> [Puzzle] -- {{{2
solve = go where
    go p = case nextLoc p of
        Fail -> []
        Done -> [p]
        Next e x s ->
            concat $ map (go . setValue p e x) (S.toList s)

solveCount :: Puzzle -> [(Z,Puzzle)] -- {{{2
solveCount = flip evalState 0 . go where
    go p = case nextLoc p of
        Fail -> pure []
        Done -> (:[]).(,p) <$> get
        Next e x s -> modify succ >>
            concat <$> traverse (go . setValue p e x) (S.toList s)

nextLoc :: Puzzle -> Next -- {{{2
nextLoc Puzzle {..}
    | null cands = Done
    | S.null s   = Fail
    | otherwise  = Next e x s
  where
    (s,(e,x)) = minimumBy (comparing $ S.size.fst) cands
    cands =
        [ (s,(e,x))
        | (e,(locs,cands)) <- M.toList _eqns
        , let evals = S.fromList (concat cands)
        , x@(r,c) <- locs
        , let s = _rows!r /\ _cols!c /\ evals
        ]

setValue :: Puzzle -> Eid -> Loc -> Z -> Puzzle -- {{{2
setValue Puzzle {..} e x@(r,c) v = p
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

-- input/output -- {{{1
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
            '+' -> multi 0 (+) (*)
            '*' -> multi 1 (*) (^)
            '-' -> two (+)
            '/' -> two (*)
            c -> error $ "invalid operator: " ++ show c
        multi z (#) (##) = go [] 1 z n where
            go xx x u n
                | n == 0 && u == v = [xx]
                | n == 0 = []
                | u # (x ## n) > v = []
                | u # (sz ## n) < v = []
                | otherwise = concatMap f [x..sz]
              where f i = go (i:xx) i (i#u) (n-1)
        two (#) = if n /= 2 then [] else
            [ [a,b] | a <- [1..sz-1], b <- [a+1..sz], a # v == b ]

putPuzzle :: Puzzle -> IO () -- {{{2
putPuzzle Puzzle {..} = traverse_ putStrLn soln where
    soln = chunksOf sz $ intToDigit.snd <$> sort _vals
    sz = V.length _rows

main :: IO () -- {{{1
main = getPuzzle >>= out . solveCount where
    out [] = hPutStrLn stderr "IMPOSSIBLE"
    out ((n,p):_) = do
        hPutStrLn stderr $ printf "%d decisions" n
        putPuzzle p
