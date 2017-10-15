{-# LANGUAGE BangPatterns #-}
module ChessTest where

import Chessboard

start_pieces :: [[Char]]
start_pieces = 
  ["BR","BN","BB","BK","BQ","BB","BN","BR"] ++ 
  ["BP","BP","BP","BP","BP","BP","BP","BP"] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["WP","WP","WP","WP","WP","WP","WP","WP"] ++ 
  ["WR","WN","WB","WQ","WK","WB","WN","WR"]


check_pieces :: [[Char]]
check_pieces = 
  ["BK","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["WQ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "]

check :: State
check = (State (makeBoard check_pieces) Black)


checkmate_pieces :: [[Char]]
checkmate_pieces = 
  ["BK","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["WQ","WR","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "]

checkmate :: State
checkmate = (State (makeBoard checkmate_pieces) Black)
