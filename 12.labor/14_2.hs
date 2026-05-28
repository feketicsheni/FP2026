import Data.List

data Film = Film
  { cím :: [Char],
    fRendezo :: [Char],
    fEv :: Int,
    fKoltseg :: Int
  }
  deriving (Show)

data Bfa
  = Ures
  | Csomopont Film Bfa Bfa
  deriving (Show)

-- Sor feldolgozása
-- Titanic;Cameron;1997;200
parseFilm :: String -> Film
parseFilm sor =
  let d = split ';' sor
   in Film
        (d !! 0)
        (d !! 1)
        (read (d !! 2))
        (read (d !! 3))

split :: Char -> String -> [String]
split _ [] = [""]
split c (x : xs)
  | x == c = "" : rest
  | otherwise = (x : head rest) : tail rest
  where
    rest = split c xs

-- Beszúrás
insertFilm :: Film -> Bfa -> Bfa
insertFilm f Ures = Csomopont f Ures Ures
insertFilm f (Csomopont x bal jobb)
  | fEv f < fEv x = Csomopont x (insertFilm f bal) jobb
  | otherwise = Csomopont x bal (insertFilm f jobb)

-- Fa építése
epitFa :: [Film] -> Bfa
epitFa = foldr insertFilm Ures

-- Filmek adott évben
filmekEvben :: Int -> Bfa -> [Film]
filmekEvben _ Ures = []
filmekEvben ev (Csomopont x bal jobb)
  | fEv x == ev = x : filmekEvben ev bal ++ filmekEvben ev jobb
  | ev < fEv x = filmekEvben ev bal
  | otherwise = filmekEvben ev jobb

-- Inorder bejárás
inorder :: Bfa -> [Film]
inorder Ures = []
inorder (Csomopont x bal jobb) =
  inorder bal ++ [x] ++ inorder jobb

-- Legnagyobb költség
maxKoltseg :: [Film] -> Int
maxKoltseg lista = maximum (map fKoltseg lista)

legdragabbak :: [Film] -> [Film]
legdragabbak lista =
  let m = maxKoltseg lista
   in filter (\x -> fKoltseg x == m) lista

main :: IO ()
main = do
  tartalom <- readFile "film.txt"
  let filmek = map parseFilm (lines tartalom)

  let fa = epitFa filmek

  putStrLn "Év:"
  ev <- readLn

  putStrLn "Filmek:"
  print (filmekEvben ev fa)

  putStrLn "Inorder:"
  print (inorder fa)

  putStrLn "Legdrágább filmek:"
  print (legdragabbak filmek)