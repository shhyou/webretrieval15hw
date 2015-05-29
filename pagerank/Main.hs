{-# LANGUAGE BangPatterns #-}

module Main where

import System.IO (stderr, hPutStrLn, hPutStr, hPutChar, hPrint, withFile, IOMode(..))
import System.Exit (exitFailure)
import System.Environment (getArgs, withArgs)

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

optLessL2Norm = True

type Vector = UArray Int Double
type Graph  = Array Int (UArray Int Int)

data PageRank = PageRank { eps :: Double
                         , damp :: Double
                         , numNodes :: Int
                         , outWeight :: UArray Int Double
                         , sinkNodes :: [Int]
                         , sinkNodesWeight :: Double
                         , inEdges :: Graph }

-- pⱼ = (1 - d) + d Σᵢ 1 / Oᵢ pᵢ
nextRank :: PageRank -> Vector -> Vector
nextRank page rank = I.accumArray (+) baseValue (1, numNodes page) $
  (++)
    (map (\u -> (u, -sinkWeight*rank!u)) $ sinkNodes page)
    [ (u, sum [ rank!v * outWeight page!v | v <- I.elems (inEdges page!u) ])
    | u <- I.indices rank ]
  where !baseValue = 1 - damping + sinkWeight * (sum . map (rank!) . sinkNodes $ page)
        sinkWeight = sinkNodesWeight page
        damping = damp page

l2norm2 :: (Vector, Vector) -> Double
l2norm2 (v1, v2) = sum [((v1!i) - (v2!i))*((v1!i) - (v2!i)) | i <- I.indices v1 ]

pageRank :: PageRank -> [Vector]
pageRank page = map snd . takeWhile ((> epsilon*epsilon) . l2norm2) $ zip ranks' (tail ranks')
  where ranks = iterate (nextRank page) (I.listArray (1, numNodes page) (repeat 1.0))
        ranks' = (if optLessL2Norm then every5 else id) ranks
        every5 xs = z:every5 zs where z:zs = drop 4 xs
        epsilon = eps page

graphParser :: Parser (Int, Graph)
graphParser = do
  n <- string (pack "#maxnode") *> skipSpace *> decimal <?> "#maxnode"
  g <- I.accumArray (const id) (I.listArray (0, -1) []) (1, n) <$> many' (do
    (u, m) <- (,) <$> (skipSpace *> decimal <* char ':') <*> decimal
    ns <- I.listArray (0, m-1) <$> count m (skipSpace *> decimal)
    return (u, ns))
  return (n, g)

data Config = Config { inputFile :: String
                     , outputFile :: String
                     , damping :: Double
                     , epsilon :: Double }
            deriving (Show)

defaultConfig = Config { inputFile = error "No input file specified"
                       , outputFile = "output.pagerank"
                       , damping = 0.85
                       , epsilon = 1e-6 }

parseOpts :: Config -> [String] -> Config
parseOpts c [input]         = c { inputFile = input }
parseOpts c ("-o":out:args) = parseOpts (c { outputFile = out }) args
parseOpts c ("-e":e:args)   = parseOpts (c { epsilon = read e }) args
parseOpts c ("-d":d:args)   = parseOpts (c { damping = read d }) args

main = do
  config@(Config { inputFile = input
                 , outputFile = output
                 , damping = d
                 , epsilon = e }) <- parseOpts defaultConfig <$> getArgs
  hPutStrLn stderr $ "Running with config '" ++ show config ++ "'"
  hPutStrLn stderr $ "Reading " ++ input
  res <- parse graphParser <$> L.readFile input
  (n, g) <- case res of
    failed@(Fail _ _ _) -> print failed >> exitFailure
    Done _ ng -> return ng
  hPutStrLn stderr "Degrees..."
  let !invOutDegs = I.listArray (1, n) . map ((d / ) . fromIntegral . rangeSize . I.bounds) $ I.elems g
  inDegrees <- M.newArray (1, n) 0 :: IO (IOUArray Int Int)
  sequence_ . map (\i -> M.writeArray inDegrees i . (+1) =<< M.readArray inDegrees i) . concatMap I.elems . I.elems $ g
  hPutStrLn stderr "Sink nodes..."
  let !sinks = map fst . filter ((== 0) . rangeSize . I.bounds . snd) . I.assocs $ g
  hPutStrLn stderr ("Total " ++ show (length sinks) ++ " sink node(s).")
  hPutStrLn stderr "Reversing graph..."
  ginv' <- M.newListArray (1, n) =<< mapM (\d -> M.newArray (0, d-1) 0) =<< M.getElems inDegrees
             :: IO (IOArray Int (IOUArray Int Int))
  let rev_loop 0 = return ()
      rev_loop u = do
        forM_ (I.elems (g!u)) $ \v -> do
          deg <- M.readArray inDegrees v
          M.writeArray inDegrees v (deg-1)
          inNodes <- M.readArray ginv' v
          M.writeArray inNodes (deg-1) u
        rev_loop (u-1)
  rev_loop n
  hPutStrLn stderr "Freezing array..."
  ginv <- I.listArray (1, n) <$> (mapM M.freeze =<< M.getElems ginv')
  hPutStr stderr "Calculating PageRank"
  let !ranks = pageRank $ PageRank { eps = e
                                   , damp = d
                                   , numNodes = n
                                   , outWeight = invOutDegs
                                   , sinkNodes = sinks
                                   , sinkNodesWeight = d / fromIntegral (n-1)
                                   , inEdges = ginv }
  let loop [r]    = return r
      loop (r:rs) = hPutChar stderr '.' >> loop rs
  rank <- loop ranks
  hPutStrLn stderr "\nOutputing..."
  withFile output WriteMode $ \fout ->
    forM_ (range (I.bounds rank)) $ \i -> do
      hPutStr fout (show i)
      hPutChar fout ':'
      hPrint fout (rank!i)

test :: String -> IO ()
test filename = withArgs ["-o", "test.rank", filename] main >> readFile "test.rank" >>= putStr

