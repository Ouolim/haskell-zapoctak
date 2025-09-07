import Data.Char (toLower)
import Data.List (minimumBy, delete, nub)
import System.IO (hFlush, stdout)
import Data.Ord (comparing)
import qualified Data.Set as S

-- Board coords: (file 1..8 for a..h, rank 1..8)
type P = (Int,Int)
files = "abcdefgh"

------- utilities ---------
inBounds (x,y) = x>=1 && x<=8 && y>=1 && y<=8
parseSq :: String -> Maybe P
parseSq [f,r]
  | f' >=1 && f'<=8 && r' >=1 && r'<=8 = Just (f', r')
  where f' = maybe 0 (+1) (lookup f (zip files [0..]))
        r' = fromEnum r - fromEnum '0'
parseSq _ = Nothing

showSq (x,y) = files !! (x-1) : show y

-- Pretty board print
printBoard wk qpos bk = do
  putStrLn "  a b c d e f g h"
  mapM_ printRank [8,7..1]
  where printRank r = do
          putStr (show r ++ " ")
          mapM_ (printCell r) [1..8]
          putStrLn ""
        printCell r f
          | (f,r) == wk = putStr "K "
          | (f,r) == qpos = putStr "Q "
          | (f,r) == bk = putStr "k "
          | otherwise = putStr ". "


---- Chess utilities ----
-- distance (Chebyshev), used for distance from kings
dist (x1,y1) (x2,y2) = max (abs (x1-x2)) (abs (y1-y2))

kingMoves p = filter inBounds [(x+dx,y+dy) | dx<-[-1..1], dy<-[-1..1], (dx,dy) /= (0,0)]
  where (x,y)=p

-- Queen moves (rays) until blocked by board edge or by white king.
queenRays (qx,qy) wk = concatMap (ray (qx,qy)) directions
  where directions = [(1,0),(-1,0),(0,1),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)]
        ray (x,y) (dx,dy) = go (x+dx,y+dy)
          where go p@(nx,ny)
                  | not (inBounds p) = []
                  | p == wk = [] -- can't pass through white king
                  | otherwise = p : go (nx+dx, ny+dy)

-- Squares attacked by queen (same as queenRays but including the line up to blocking square)
queenAttacks (qx,qy) wk bk = concatMap (att (qx,qy)) directions
  where directions = [(1,0),(-1,0),(0,1),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)]
        att (x,y) (dx,dy) = go (x+dx,y+dy)
          where go p@(nx,ny)
                  | not (inBounds p) = []
                  | p == wk = [] -- line blocked by white king
                  | otherwise = p : if p == bk then [] else go (nx+dx, ny+dy)

-- Check if square attacked by white (queen or king)
isAttackedByWhite sq qpos kpos = sq `elem` qAtt || sq `elem` kAtt
  where qAtt = queenAttacks qpos kpos sq -- note: queenAttacks takes bk to stop at it
        kAtt = kingMoves kpos

-- BFS for black freedom.
-- We count number of distinct squares reachable by black king starting from its current square
-- without stepping onto squares occupied by White pieces (queen or king). We allow revisiting but set tracks.
blackFreedom :: P -> P -> P -> Int
blackFreedom bkPos wk qpos = S.size visited
  where
    blocked = S.fromList [wk, qpos]
    attackedByQueen = S.fromList $ queenAttacks qpos wk bkPos
    forbidden = S.union blocked attackedByQueen
    start = bkPos
    visited = bfs forbidden (S.singleton start) [start]

    bfs _ vis [] = vis
    bfs forbidden vis (p:ps) =
      let next = [n | n <- kingMoves p, inBounds n, not (n `S.member` forbidden), not (n `S.member` vis)]
      in bfs forbidden (S.union vis (S.fromList next)) (ps ++ next)
  
-- Generate candidate queen moves 
allQueenCandidates qpos wk bk = nub [p | p <- queenRays qpos wk, p /= bk]

-- queen is capturable when adjacent to black king and not defended by white king
queenCapturable qpos wk bk = (dist qpos bk == 1) && (dist qpos wk > 1)

whiteKingCandidates wk qpos bk = filter ok $ wk : kingMoves wk
  where ok p = inBounds p && p /= qpos && dist p bk > 1 -- king cannot move adjacent to other king

attackedByBlack sq bk = dist sq bk == 1

legalBlackMoves bk wk qpos = filter legal $ bk : kingMoves bk
  where occupied = S.fromList [wk,qpos, bk]
        -- square is attacked by queen if it's in queenAttacks
        qAtt = queenAttacks qpos wk bk
        legal s = inBounds s && s /= wk && s /= qpos && s /= bk && dist s wk > 1 && not (s `elem` qAtt)

blackInCheck bk wk qpos = bk `elem` queenAttacks qpos wk bk || dist bk wk == 1

-- checkmate / stalemate
isCheckmate bk wk qpos = null (legalBlackMoves bk wk qpos) && blackInCheck bk wk qpos
isStalemate bk wk qpos = null (legalBlackMoves bk wk qpos) && not (blackInCheck bk wk qpos)

-- Evaluate queen move: compute freedom after move, but also check queen safety & avoid stalemate
scoreQueenMove cand wk bk =
  let freedom = blackFreedom bk wk cand
      capturable = queenCapturable cand wk bk
      stal = isStalemate bk wk cand
  in if capturable then (1000, freedom, True) else if stal then (900, freedom, False) else (freedom, freedom, False)
  -- we return tuple prioritized so we can choose min properly: prefer smallest freedom; but huge penalty if capturable

chooseQueenMove :: P -> P -> P -> P
chooseQueenMove qpos wk bk =
  let cands = allQueenCandidates qpos wk bk
      scored = [(cand, scoreQueenMove cand wk bk) | cand <- cands]
      -- remove candidates where bk can capture wq
      nonCap = filter (not . (\(_,(_,_,cap))->cap)) scored
      pool = if null nonCap then scored else nonCap
      best = minimumBy (comparing (\(cand,(f,_,_)) -> (f, dist cand bk))) pool
  in fst best

-- move white king toward black king - reducing distance
moveWhiteKingCloser wk qpos bk =
  let cands = whiteKingCandidates wk qpos bk
      better = filter (\p -> dist p bk < dist wk bk) cands
  in case better of
       [] -> wk
       _  -> minimumBy (comparing (`dist` bk)) better
gameLoop :: P -> P -> P -> IO ()
gameLoop wk qpos bk = do
  -- White AI
  let freedomNow = blackFreedom bk wk qpos
  let mateCands = filter (\cand -> isCheckmate bk wk cand && not (queenCapturable cand wk bk))
                         (allQueenCandidates qpos wk bk)
  (wk', qpos') <-
    -- 1) if white can checkmate, than checkmate
    if not (null mateCands)
      then return (wk, head mateCands)
    -- 2) if king is in the corner (2 free squares) than:
    else if freedomNow == 2
      then do
        let wkNew = moveWhiteKingCloser wk qpos bk
        -- check if stalemate after king move, move queen to better square
        if isStalemate bk wkNew qpos
          then do
            return (wk, chooseQueenMove qpos wk bk)
        -- else move king normally
          else return (wkNew, qpos)
    else do
    -- 3) else pick move that restricts black king the most
      let qcand = chooseQueenMove qpos wk bk
      return (wk, qcand)

  -- Print board after White move
  putStrLn ""
  printBoard wk' qpos' bk

  -- Check for mate or stalemate
  if isCheckmate bk wk' qpos'
    then putStrLn "Checkmate! White wins."
  else if isStalemate bk wk' qpos'
   then putStrLn "Stalemate! It's a draw."
   else do
     -- Black move
     putStr "Black move (e.g. e4): "
     hFlush stdout
     line <- getLine
     case parseSq (map toLower (filter (not . (`elem` " \t\n\r")) line)) of
       -- if invalid, run the game loop again with old state
       Nothing -> do
         putStrLn "Invalid square format."
         gameLoop wk qpos bk
       Just bNew -> do
         let legal = bNew `elem` legalBlackMoves bk wk' qpos'
         if not legal
           then do
             -- if illegal (moved to check,..), run the game loop again with old state
             putStrLn "Illegal move for Black (square occupied/into check/too far). Try again."
             gameLoop wk qpos bk
           else do
             putStrLn $ "Black moves: " ++ showSq bNew
             gameLoop wk' qpos' bNew

-- input helper: ask for position or random
getPosition prompt = do
  putStr prompt
  hFlush stdout
  s <- getLine
  if null s then return Nothing else return (parseSq (map toLower (filter (not . (`elem` " \t\n\r")) s)))

defaultStart = ( (5,1) -- white king e1
               , (4,4) -- white queen d4
               , (3,8) -- black king c8
               )
main :: IO ()
main = do
  putStrLn "Q+K vs K simulator. Provide positions as e2, a1, etc. Press enter for default."
  mWk <- getPosition "White King (default e1): "
  mQ  <- getPosition "White Queen (default d4): "
  mBk <- getPosition "Black King (default c8): "
  let (wk0,q0,bk0) = case (mWk,mQ,mBk) of
                       (Just a, Just b, Just c) -> (a,b,c)
                       _ -> defaultStart
  putStrLn $ "Starting: WK=" ++ showSq wk0 ++ " Q=" ++ showSq q0 ++ " BK=" ++ showSq bk0
  gameLoop wk0 q0 bk0
