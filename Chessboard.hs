import Data.Array
import Data.Maybe

import Debug.Trace

--module Chessboard where

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King 
               deriving (Eq, Ord, Bounded)

data Side = White | Black deriving (Eq)

data Piece = Piece {   pieceType :: PieceType
                     , side :: Side } deriving (Eq)

newtype Square = Square (Maybe Piece)

type Board = Array (Int, Int) Square

type Move = (Int, Int)

type PieceIndex = (Int, Int)

type PieceMove = (PieceIndex, Move)

data State = State { board :: Board
                   , toMove :: Side
                   }
             
start_pieces :: [[Char]]
start_pieces = 
  ["BR","BN","BB","BK","BQ","BB","BN","BR"] ++ 
  (replicate 8 "BP") ++
  (replicate 8 "  ") ++
  (replicate 8 "  ") ++
  (replicate 8 "  ") ++
  (replicate 8 "  ") ++
  (replicate 8 "WP") ++
  ["WR","WN","WB","WQ","WK","WB","WN","WR"]
  
    
makeBoard :: [[Char]] -> Board
makeBoard pieces = listArray ((1,1),(8,8)) (map makeSquare pieces)

start_board :: Board
start_board = makeBoard start_pieces

start_state :: State
start_state = State { board = start_board,
                toMove = White
                }


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
  ["WR","WQ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "] ++ 
  ["  ","  ","  ","  ","  ","  ","  ","  "]

checkmate :: State
checkmate = (State (makeBoard checkmate_pieces) Black)


pieceValue :: Piece -> Int
pieceValue p
  | pt == King = 100000
  | pt == Queen = 9
  | pt == Rook = 5
  | pt == Bishop = 3
  | pt == Knight = 3
  | otherwise = 1
  where pt = pieceType p


squareToPiece :: Square -> Piece
squareToPiece (Square s) = (fromJust s)

squareValue :: Square -> Int
squareValue (Square Nothing) = 0
squareValue (Square (Just s)) = pieceValue s

makeSide :: Char -> Side
makeSide s 
  | s == 'W' = White
  | s == 'B' = Black
               
oppositeSide :: Side -> Side
oppositeSide White = Black
oppositeSide Black = White
  
makePieceType :: Char -> PieceType
makePieceType s
  | s ==  'K' = King
  | s ==  'Q' = Queen
  | s ==  'R' = Rook  
  | s ==  'B' = Bishop
  | s ==  'N' = Knight
  | s ==  'P' = Pawn
  
getPiece :: [Char] -> Piece
getPiece [s0, s1] = (Piece (makePieceType s1) (makeSide s0))

makeSquare :: [Char] -> Square
makeSquare "  " = (Square Nothing)
makeSquare x = (Square (Just (getPiece x)))

possibleMoves :: Piece -> [Move] -- y, x
possibleMoves p 
  | pt == King = [(a,b) | a <- [(-1)..1], b <- [(-1)..1]]
  | pt == Rook = [(0,a) | a <- [(-7)..7]] ++ [(a,0) | a <- [(-7)..7]]
  | pt == Bishop = [(a,a) | a <- [(-7)..7]] ++ [(a,(-a)) | a <- [(-7)..7]]
  | pt == Queen = (possibleMoves (Piece Rook (side p))) ++ (possibleMoves (Piece Bishop (side p)))
  | pt == Knight = [((-1),(-2)),
                        ((-1),2),
                        ((-2),(-1)),
                        ((-2),1),
                        (1,2),
                        (1,-2),
                        (2,-1),
                        (2,1)]
  | pt == Pawn && ((side p) == White) = [((-1),0)]
  | pt == Pawn && ((side p) == Black) = [(1,0)]
  where
    pt = pieceType p

getPieceAt :: PieceIndex -> Board -> Piece
getPieceAt pi b = (squareToPiece (b ! pi))

containsPiece ::  PieceIndex -> Board -> Bool
--containsPiece pi b = trace ("Checking if " ++ (show pi) ++ " contains piece\n") $ squareValue (b ! pi) > 0
containsPiece pi b = squareValue (b ! pi) > 0

checkMoveBounds :: PieceMove -> Bool
checkMoveBounds ((x,y), (a,b)) = ((x + a) > 0) && ((x + a) < 9) && ((y + b) > 0) && ((y + b) < 9)

unitVector :: Int -> Int
unitVector a 
  | a > 0 = 1
  | a < 0 = (-1)
  | otherwise = 0

getIntermediateMoves :: PieceMove -> [Move]
getIntermediateMoves (_, (0,0)) = []
getIntermediateMoves ((x,y), (a,b)) = 
  let ua = unitVector a
      ub = unitVector b
  in 

   [((x+ua),(y+ub))] ++ 
--      (trace ("\nx,y" ++ (show (x,y)) ++ "\n"
--   ++ "a,b" ++ (show (x,y)) ++ "\n"))
   getIntermediateMoves ((x+ua,y+ub), ((a-ua),(b-ub)))

checkPossibleMove :: PieceMove -> Board -> Bool
checkPossibleMove ((0,0), _) _ = True
checkPossibleMove ((x,y), (a,b)) board = 
  let 
    target = ((x+a),(y+b))
    --source = (trace ("x is " ++ (show (x,y)) ++ "target is " ++ (show target)) (getPieceAt (x,y) board))
    source = (getPieceAt (x,y) board)
  in 
   -- Make sure we're not moving off the board.
   (checkMoveBounds ((x,y), (a,b))) &&
   -- Make sure we're not moving onto our own piece.
   ((not (containsPiece target board)) || (not ((side source) == side (getPieceAt target board))))
     && (((pieceType source) == Knight)
         -- Make sure there are no pieces in the way.
         || (foldr (&&) True (map (\(a,b) -> not (containsPiece ((a,b)) board)) 
                              (filter (not . (== target)) 
                               (getIntermediateMoves ((x,y), (a,b)))))))


movesForPiece :: PieceIndex -> Board -> [PieceMove]
movesForPiece pi b = [(pi, x) | x <- filter (\x -> checkPossibleMove (pi, x) b) $ possibleMoves $ getPieceAt pi b]

boardPiecesIdx :: Side -> Board -> [PieceIndex]
boardPiecesIdx s b = filter (\x -> (containsPiece x b) && (side (getPieceAt x b) == s)) [(a,b) | a <- [1..8], b <- [1..8]]

allValidMoves :: State -> [PieceMove]
allValidMoves s = concat $ map (\x -> movesForPiece x (board s)) $ boardPiecesIdx (toMove s) (board s)

allLegalMoves:: State -> [PieceMove]
allLegalMoves s 
  | checkMate s = []
  | inCheck s = filter (\z -> not (inCheck (State (applyMove z (board s)) (toMove s)))) $
                allValidMoves s
  | otherwise = allValidMoves s

inCheck :: State -> Bool
inCheck state = length (filter (\((x,y), (a,b)) -> (containsPiece (x+a,y+b) (board state)) && 
                                                   (getPieceAt (x+a,y+b) (board state)) == (Piece King (toMove state))) (allValidMoves (State (board state) (oppositeSide (toMove state))))) > 0

checkMate :: State -> Bool
checkMate s = inCheck s && (foldr (&&) True $ map (\x -> inCheck (State (board x) (toMove s))) $ nextStates s)

nextStates :: State -> [State]
nextStates s = map (\b -> (State b (oppositeSide (toMove s)))) $ map (\x -> applyMove x (board s)) $ allValidMoves s

applyMove :: PieceMove -> Board -> Board
applyMove ((x,y), (a,b)) board = board // [((x,y), (Square Nothing)),
                                           ((x+a,y+b), board!(x,y))]

instance Show Side where
  show Black = "B"
  show White = "W"
  
instance Show PieceType where
  show King = "K"
  show Queen = "Q"
  show Rook = "R"
  show Bishop = "B"
  show Knight = "N"
  show Pawn = "P"

instance Show Piece where
  show p = Prelude.show (side p) ++ Prelude.show (pieceType p)

instance Show Square where
  show (Square Nothing) = "  |"
  show (Square (Just p)) =  (Prelude.show p) ++ "|"
  
showpm :: PieceMove -> Board -> String
showpm ((x,y), (a,b)) board = (show (getPieceAt (x,y) board)) ++ " from " ++ (show (x,y)) ++ " to " ++ (show ((x+a),(y+b)))

showMoves :: State -> [String]
showMoves s = map (\x -> showpm x (board s)) $ allLegalMoves s 

-- displayMoves :: [PieceMove] -> Board -> String
-- displayMoves pms b = map (\x -> (showpm x b) ++ "\n") pms 

getNextMove :: State -> PieceMove

instance Show State where
  show s = "\n   " ++ (concat [" " ++ (show x) ++ " " | x <- [1..8]]) ++ "\n  " ++ (replicate 25 '-') ++ "\n" ++ pbh (elems (board s)) ++ "   " ++ (show (toMove s)) ++ " to move." ++ (if (inCheck s) then " Check." else "")
    where
      pbh b
        | length b > 0 = (show (round (9 - (fromIntegral (length b))/8))) ++ " |" ++ (concat (map Prelude.show (take 8 b))) ++ "\n  " ++ (replicate 25 '-') ++ "\n" ++ (pbh (drop 8 b))
        | otherwise = ""

boardPieces :: Board -> [Piece]
boardPieces b = map (\(Square (Just x)) -> x) $ filter (\x -> (squareValue x) > 0) (elems b)

boardValue :: Board -> Int
boardValue b = (bvf White b) - (bvf Black b)
  where
    bvf s b = sum $ map pieceValue (filter (\x -> side x == s) (boardPieces b))

