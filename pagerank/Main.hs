module Main where

import System.Exit (exitFailure)
import System.Environment (getArgs)

import Control.Applicative ((<$>), Applicative(..))

import qualified Data.Array as A
import qualified Data.Array.IArray as I
import           Data.Array.IArray ((!))
import qualified Data.Array.Unboxed as U

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 (pack)

import Data.Attoparsec.ByteString.Char8 (char, decimal, string, skipSpace, many', count, (<?>))
import Data.Attoparsec.ByteString.Lazy (Parser(), Result(..), parse)

type Vector = U.UArray Int Double
type Graph  = A.Array Int (U.UArray Int Int)


epsilon = 1e-6
damping = 0.85

data PageRank = PageRank { numNodes :: Int
                         , numNodesD :: Double
                         , outDegreeD :: U.UArray Int Double
                         , sinkNodes :: U.UArray Int Int
                         , inEdges :: Graph }

-- pⱼ = (1 - d) + d Σᵢ 1 / Oᵢ pᵢ
nextRank :: PageRank -> Vector -> Vector
nextRank page rank = I.listArray (0, numNodes page - 1)
  [ baseValue + damping * sum [ rank!v / outDegreeD page!v
                              | v <- I.elems (inEdges page!u) ]
  | u <- I.indices rank ]
  where baseValue = 1 - damping + 1 / numNodesD page * (sum . map (rank!) . I.elems . sinkNodes $ page)

l2norm2 :: (Vector, Vector) -> Double
l2norm2 (v1, v2) = sum [((v1!i) - (v2!i))*((v1!i) - (v2!i)) | i <- I.indices v1 ]

pageRank :: PageRank -> Vector
pageRank page = snd . last . takeWhile ((> epsilon*epsilon) . l2norm2) $ zip ranks (tail ranks)
  where ranks = iterate (nextRank page) (I.listArray (0, numNodes page - 1) [1.0/numNodesD page..])

graphParser :: Parser Graph
graphParser = do
  n <- string (pack "#maxnode") *> skipSpace *> decimal <?> "#maxnode"
  I.array (0,n) <$> many' (do
    (u, m) <- (,) <$> (skipSpace *> decimal <* char ':') <*> decimal
    ns <- I.listArray (0,m-1) <$> count m (skipSpace *> decimal)
    return (u, ns))

main = do
  [input, output] <- getArgs
  res <- parse graphParser <$> L.readFile input
  g <- case res of
    failed@(Fail _ _ _) -> print failed >> exitFailure
    Done _ g -> return g
  return ()
