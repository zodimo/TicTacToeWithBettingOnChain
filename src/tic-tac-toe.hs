{-# LANGUAGE TypeApplications #-}

-- pure code
type Size   = Int
data Tick   = X | O | E deriving (Eq, Read, Show)
type Row    = [Tick]
type Board  = [[Tick]]

mkEmptyBoard :: Size -> Board
mkEmptyBoard n = replicate n (replicate n E)

flipTick :: Tick -> Tick
flipTick X = O
flipTick O = X

getRow :: Int -> Board -> Row
getRow 0 (xs : xss) = xs
getRow n (xs : xss) = getRow (n - 1) xss

getTick :: (Int, Int) -> Board -> Tick
getTick (0, n) (xs : xss) = xs !! n
getTick (m, n) (xs : xss) = getTick (m - 1, n) xss

transpose :: Board -> Board
transpose ([] : _)  = []
transpose xss       = (map head xss) : (transpose (map tail xss))

trace :: Board -> Row
trace []          = []
trace (xs : xss)  = head xs : trace (map tail xss)

antitrace :: Board -> Row
antitrace []          = []
antitrace (xs : xss)  = last xs : antitrace (map init xss)

instance Show Tick where
  show :: Tick -> String
  show E = "_"
  show X = "x"
  show O = "o"

changeRow :: Tick -> Int -> Row -> Row
changeRow y 0 (x : xs)  = y : xs
changeRow y n (x : xs)  = x : (changeRow y (n - 1) xs)

playMove :: Tick -> (Int, Int) -> Board -> Board
playMove y (0, n) (xs : xss) = (changeRow y n xs) : xss
playMove y (m, n) (xs : xss) = xs : (playMove y (m - 1, n) xss)

horizMate :: Tick -> Board -> Bool
horizMate _ []          = False
horizMate x (xs : xss)  = (all (== x) xs) || (horizMate x xss)

vertMate :: Tick -> Board -> Bool
vertMate x xss = horizMate x (transpose xss)

diagMate :: Tick -> Board -> Bool
diagMate x xss = all (== x) (trace xss) || all (== x) (antitrace xss)

-- make it work less
gameOver :: Board -> (Bool, Tick)
gameOver b
  | horizMate X b = (True, X)
  | horizMate O b = (True, O)
  | vertMate X b  = (True, X)
  | vertMate O b  = (True, O)
  | diagMate X b  = (True, X)
  | diagMate O b  = (True, O)
  | otherwise     = (False, E)

isBoardFull :: Board -> Bool
isBoardFull b = all (/= E) (concat b)
-- pure code

-- impure code
showRow :: Row -> IO ()
showRow [x]       = putStrLn $ show x
showRow (x : xs)  = putStr (show x) >> putChar ' ' >> showRow xs

showBoard :: Board -> IO ()
showBoard = mapM_ showRow

playGame :: Size -> Tick -> Board -> IO ()
playGame s x b = do
  showBoard b
  putStrLn $ "Player " ++ (show x) ++ " shall move."
  p <- getLine
  let m = read $ [head p]; n = read $ [last p]
  case getTick (m, n) b of
    O -> do
      putStrLn $ "Player " ++ (show x) ++ " can only fill empty squares."
      playGame s x b
    X -> do
      putStrLn $ "Player " ++ (show x) ++ " can only fill empty squares."
      playGame s x b
    E -> do
      let c = playMove x (m, n) b
      case gameOver c of
        (False, _)  -> do
          case isBoardFull c of
            True  -> do
              showBoard c
              putStrLn "Players X and O have drawn the game."
            False -> playGame s (flipTick x) c
        (True, X)   -> do
          showBoard c
          putStrLn "Player X has won the game."
        (True, O)   -> do
          showBoard c
          putStrLn "Player O has won the game."

main :: IO ()
main = do
  putStrLn "Enter the size of the board:"
  s <- fmap read getLine
  putStrLn "Enter the first player:"
  player <- getLine
  case player of
    "X" -> playGame s X (mkEmptyBoard s)
    "O" -> playGame s O (mkEmptyBoard s)
-- impure code
