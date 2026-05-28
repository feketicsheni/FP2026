import Data.List
import System.IO

data Beteg = Beteg
  { bNev :: [Char],
    bVerny :: [(Int, Int)],
    bSzEv :: Int
  }
  deriving (Show)

-- Sor feldolgozása
-- Példa sor:
-- Bela;1980;120 80;170 100;150 90
parseBeteg :: String -> Beteg
parseBeteg sor =
  let darabok = split ';' sor
      nev = darabok !! 0
      szev = read (darabok !! 1) :: Int
      verny = map parsePair (drop 2 darabok)
   in Beteg nev verny szev

parsePair :: String -> (Int, Int)
parsePair s =
  let [a, b] = words s
   in (read a, read b)

split :: Char -> String -> [String]
split _ [] = [""]
split c (x : xs)
  | x == c = "" : rest
  | otherwise = (x : head rest) : tail rest
  where
    rest = split c xs

-- Rendezés név szerint
rendezNevSzerint :: [Beteg] -> [Beteg]
rendezNevSzerint = sortBy (\a b -> compare (bNev a) (bNev b))

-- Bináris keresés
binarySearch :: [Beteg] -> String -> Maybe Beteg
binarySearch [] _ = Nothing
binarySearch lista nev = keres 0 (length lista - 1)
  where
    keres bal jobb
      | bal > jobb = Nothing
      | otherwise =
          let kozep = (bal + jobb) `div` 2
              beteg = lista !! kozep
           in case compare nev (bNev beteg) of
                EQ -> Just beteg
                LT -> keres bal (kozep - 1)
                GT -> keres (kozep + 1) jobb

-- Magas vérnyomások száma
magasDb :: Beteg -> Int
magasDb beteg =
  length [(x, y) | (x, y) <- bVerny beteg, x > 160 || y > 140]

main :: IO ()
main = do
  tartalom <- readFile "betegek.txt"
  let betegek = map parseBeteg (lines tartalom)
  let rendezett = rendezNevSzerint betegek

  putStrLn "Beteg neve:"
  nev <- getLine

  case binarySearch rendezett nev of
    Nothing -> putStrLn "Nincs ilyen beteg!"
    Just b -> do
      print (bVerny b)
      putStrLn ("Magas vérnyomások száma: " ++ show (magasDb b))