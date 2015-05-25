{-# LANGUAGE BangPatterns #-}

module Main where

import System.Exit (exitFailure)
import System.Environment (getArgs)

import Control.Applicative ((<$>), Applicative(..))
import Control.Monad

import Data.Ix
import Data.Array (Array())
import Data.Array.Unboxed (UArray())
import Data.Array.IO (IOArray(), IOUArray())
import Data.Array.IArray ((!))
import qualified Data.Array.IArray as I
import qualified Data.Array.MArray as M


import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 (pack)

import Data.Attoparsec.ByteString.Char8 (char, decimal, string, skipSpace, many', count, (<?>))
import Data.Attoparsec.ByteString.Lazy (Parser(), Result(..), parse)

type Vector = UArray Int Double
type Graph  = Array Int (UArray Int Int)


epsilon = 1e-6
damping = 0.85

data PageRank = PageRank { numNodes :: Int
                         , numNodesD :: Double
                         , outDegreeD :: UArray Int Double
                         , sinkNodes :: [Int]
                         , inEdges :: Graph }

-- pⱼ = (1 - d) + d Σᵢ 1 / Oᵢ pᵢ
nextRank :: PageRank -> Vector -> Vector
nextRank page rank = I.listArray (0, numNodes page - 1)
  [ baseValue + damping * sum [ rank!v / outDegreeD page!v
                              | v <- I.elems (inEdges page!u) ]
  | u <- I.indices rank ]
  where baseValue = 1 - damping + 1 / numNodesD page * (sum . map (rank!) . sinkNodes $ page)

l2norm2 :: (Vector, Vector) -> Double
l2norm2 (v1, v2) = sum [((v1!i) - (v2!i))*((v1!i) - (v2!i)) | i <- I.indices v1 ]

pageRank :: PageRank -> Vector
pageRank page = snd . last . takeWhile ((> epsilon*epsilon) . l2norm2) $ zip ranks (tail ranks)
  where ranks = iterate (nextRank page) (I.listArray (0, numNodes page - 1) [1.0/numNodesD page..])

graphParser :: Parser (Int, Graph)
graphParser = do
  n <- string (pack "#maxnode") *> skipSpace *> decimal <?> "#maxnode"
  g <- I.accumArray (const id) (I.listArray (0,-1) []) (0,n) <$> many' (do
    (u, m) <- (,) <$> (skipSpace *> decimal <* char ':') <*> decimal
    ns <- I.listArray (0,m-1) <$> count m (skipSpace *> decimal)
    return (u, ns))
  return (n, g)

main = do
  [input, output] <- getArgs
  putStrLn $ "Reading " ++ input
  res <- parse graphParser <$> L.readFile input
  (n, g) <- case res of
    failed@(Fail _ _ _) -> print failed >> exitFailure
    Done _ ng -> return ng
  putStrLn "Degrees..."
  let !outDegrees = I.listArray (0,n) . map (fromIntegral . rangeSize . I.bounds) $ I.elems g
  inDegrees <- M.newArray (0, n) 0 :: IO (IOUArray Int Int)
  sequence_ . map (\i -> M.writeArray inDegrees i . (+1) =<< M.readArray inDegrees i) . concatMap I.elems . I.elems $ g
  putStrLn "Sink nodes..."
  let !sinks = map fst . filter ((== 0) . rangeSize . I.bounds . snd) . I.assocs $ g
  putStrLn "Reversing graph..."
  ginv' <- M.newListArray (0,n-1) =<< mapM (\d -> M.newArray (0,d-1) 0) =<< M.getElems inDegrees
             :: IO (IOArray Int (IOUArray Int Int))
  let rev_loop 0 = return ()
      rev_loop u = do
        degArray <- M.readArray ginv' u
        M.getElems degArray >>= mapM_ (\v -> do
          deg <- M.readArray inDegrees v
          M.writeArray inDegrees v (deg-1)
          M.writeArray degArray (deg-1) u)
        rev_loop (u-1)
  rev_loop (n-1)
  ginv <- I.listArray (0,n) <$> (mapM M.freeze =<< M.getElems ginv')
  let !rank = pageRank $ PageRank { numNodes = n+1
                                  , numNodesD = fromIntegral (n+1)
                                  , outDegreeD = outDegrees
                                  , sinkNodes = undefined
                                  , inEdges = ginv }
  forM_ [1..snd (I.bounds rank)] $ \i -> do
    putStr (show i)
    putChar ':'
    print (rank!i)
